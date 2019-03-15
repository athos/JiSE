(ns jise.parse
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.string :as str]
            [jise.type :as t]))

(defn symbol-without-ns [sym]
  (if (= (namespace sym) "jise.core")
    (symbol (name sym))
    sym))

(defn modifiers-of [[_ name :as form]]
  (merge (meta form) (meta name)))

(defn access-flags [modifiers]
  (cond-> #{}
    (:static modifiers) (conj :static)
    (:public modifiers) (conj :public)
    (:protected modifiers) (conj :protected)
    (:private modifiers) (conj :private)
    (:final modifiers) (conj :final)))

(defn parse-modifiers [proto-cenv {:keys [tag] :as modifiers} & {:keys [default-type]}]
  {:type (if (nil? tag)
           (or default-type t/OBJECT)
           (t/tag->type proto-cenv tag))
   :access (access-flags modifiers)})

(defn parse-field [proto-cenv [_ fname value :as field]]
  (let [modifiers (modifiers-of field)
        {:keys [access type]} (parse-modifiers proto-cenv modifiers)]
    (cond-> {:name (str fname)
             :type type
             :access access}
      (not (nil? value)) (assoc :value value))))

(defn context-of [{:keys [context]}]
  (if (= context :conditional)
    :expression
    context))

(defn with-context [x context]
  (assoc x :context context))

(defn inherit-context [x y]
  (with-context x (context-of y)))

(defn apply-conversions [conversions src]
  (reduce (fn [src {:keys [conversion to]}]
            {:op conversion
             :context (context-of src)
             :type to
             :src (assoc src :context :expression)})
          src
          conversions))

(defn macroexpand [form]
  (let [expanded (macroexpand-1 form)]
    (if (identical? expanded form)
      (if-let [[op & args] (and (seq? form) (symbol? (first form)) (namespace (first form)) form)]
        (with-meta `(. ~(symbol (namespace op)) ~(symbol (name op)) ~@args) (meta form))
        form)
      (recur (cond-> expanded
               (instance? clojure.lang.IObj expanded)
               (vary-meta merge (meta form)))))))

(declare parse-expr)

(defmulti parse-expr* (fn [cenv expr] (symbol-without-ns (first expr))))
(defmethod parse-expr* :default [cenv expr]
  (let [expanded (macroexpand expr)]
    (if-not (identical? expanded expr)
      (parse-expr cenv expanded)
      (assert false "not supported yet"))))

(defn find-lname [cenv sym]
  (get (:lenv cenv) (name sym)))

(defn parse-symbol [cenv sym]
  (letfn [(parse-as-field [cenv target]
            (parse-expr cenv (with-meta `(. ~target ~(symbol (str \- (name sym)))) (meta sym))))]
    (if-let [cname (namespace sym)]
      (parse-as-field cenv (symbol cname))
      (if-let [{:keys [index type]} (find-lname cenv sym)]
        (inherit-context {:op :local :index index :type type} cenv)
        (if-let [f (get-in cenv [:classes (:class-name cenv) :fields (name sym)])]
          (let [target (if (:static (:access f)) (:class-name cenv) 'this)]
            (parse-as-field cenv target))
          (throw (ex-info (str "unknown variable found: " sym) {:variable sym})))))))

(defn parse-ctor-invocation [cenv [op & args]]
  (let [cenv' (with-context cenv :expression)
        args' (map (partial parse-expr cenv') args)
        class (if (= op 'this)
                (t/tag->type cenv (:class-name cenv))
                (get-in cenv [:classes (:class-name cenv) :parent]))
        ctor (t/find-ctor cenv class (map :type args'))]
    {:op :ctor-invocation
     :context :statement
     :class class
     :access (:access ctor)
     :arg-types (:arg-types ctor)
     :args args'}))

(defn parse-seq [cenv expr]
  (let [expr' (if ('#{this super} (first expr))
                (parse-ctor-invocation cenv expr)
                (and (symbol? (first expr))
                     (or (when-let [lname (find-lname cenv (first expr))]
                           (when (t/array-type? (:type lname))
                             (parse-expr cenv (with-meta `(~'aget ~@expr) (meta expr)))))
                         (parse-expr* cenv expr))))]
    (as-> expr' expr'
      (if-let [line (:line (meta expr))]
        (assoc expr' :line line)
        expr')
      (if-let [label (:label (meta expr))]
        {:op :labeled :label label :target expr'}
        expr'))))

(defn parse-literal [cenv v]
  (if (nil? v)
    (inherit-context {:op :null} cenv)
    (if-let [t (t/object-type v)]
      (merge (inherit-context {:op :literal} cenv)
             (condp #(%1 %2) t
               #{t/BYTE t/SHORT t/INT t/LONG}
               {:type t/INT :value v}

               #{t/FLOAT t/DOUBLE}
               {:type t/DOUBLE :value v}

               {:type t :value v})))))

(defn parse-expr [cenv expr]
  (let [expr' (cond (symbol? expr) (parse-symbol cenv expr)
                    (seq? expr) (parse-seq cenv expr)
                    :else (parse-literal cenv expr))]
    (or (when (and (= (:context expr') :return)
                   (not= (:type expr') (:return-type cenv)))
          (when-let [cs (seq (t/assignment-conversion cenv (:type expr') (:return-type cenv)))]
            (-> (apply-conversions cs (assoc expr' :context :expression))
                (assoc :context :return))))
        expr')))

(defn  parse-exprs [cenv body]
  (let [cenv' (with-context cenv :statement)
        last' (parse-expr cenv (last body))]
    {:op :do :type (:type last')
     :exprs (-> (mapv parse-expr (repeat cenv') (butlast body))
                (conj last'))}))

(defn parse-name [proto-cenv name & {:keys [default-type]}]
  (let [{:keys [access type]} (parse-modifiers proto-cenv (meta name) :default-type default-type)]
    {:name name
     :type type
     :access access}))

(defn parse-binding [cenv lname init]
  (let [init' (some->> init (parse-expr (with-context cenv :expression)))
        lname' (parse-name cenv lname :default-type (:type init'))
        init' (if-let [cs (and init' (t/casting-conversion cenv (:type init') (:type lname')))]
                (apply-conversions cs init')
                init')]
    (-> lname'
        (update :name name)
        (assoc :index (:next-index cenv))
        (cond-> init' (assoc :init init')))))

(defn parse-bindings [cenv bindings]
  (loop [[lname init & bindings] bindings
         cenv' (with-context cenv :expression)
         ret []]
    (if lname
      (let [b (parse-binding cenv' lname init)
            cenv' (-> cenv'
                      (assoc-in [:lenv (:name b)] b)
                      (update :next-index + (t/type-category (:type b))))]
        (recur bindings cenv' (conj ret b)))
      [(inherit-context cenv' cenv) ret])))

(defn parse-method [cenv ctor? [_ mname args & body :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access type]} (parse-modifiers cenv modifiers :default-type t/VOID)
        init-lenv (if (:static access)
                    {}
                    {"this" {:index 0 :type (t/tag->type cenv (:class-name cenv))}})
        init-index (count init-lenv)
        [cenv' args'] (parse-bindings (assoc cenv :lenv init-lenv :next-index init-index)
                                      (interleave args (repeat nil)))
        return-type (if ctor? t/VOID type)
        context (if (= return-type t/VOID) :statement :return)]
    (cond-> {:return-type return-type
             :args args'
             :access access
             :body (parse-exprs (-> cenv'
                                    (assoc :return-type return-type)
                                    (with-context context))
                                body)}
      (not ctor?) (assoc :name (str mname)))))

(defn parse-supers [proto-cenv [maybe-supers & body]]
  (let [supers (when (vector? maybe-supers) maybe-supers)
        supers' (map (partial t/tag->type proto-cenv) supers)
        {[parent] false
         interfaces true} (group-by #(.isInterface (t/type->class %)) supers')]
    {:parent (seq parent)
     :interfaces interfaces
     :body (cond->> body (nil? supers) (cons maybe-supers))}))

(defn class-alias [cname]
  (symbol (str/replace cname #"^.*\.([^.]+)" "$1")))

(defn parse-class-body [cname body]
  (let [alias (class-alias cname)]
    (loop [decls body
           ret {:ctors [], :fields [], :methods []}]
      (if (empty? decls)
        ret
        (let [[decl & decls] decls]
          (if (seq? decl)
            (case (symbol-without-ns (first decl))
              def (recur decls (update ret :fields conj decl))
              defm (let [[_ name] decl]
                     (recur decls
                            (if (= name alias)
                              (update ret :ctors conj decl)
                              (update ret :methods conj decl))))
              do (recur (concat (rest decl) decls) ret)
              (if-let [v (resolve (first decl))]
                (if (:macro (meta v))
                  (recur (cons (macroexpand decl) decls) ret)
                  (let [msg (str "unknown type of declaration found: " (first decl))]
                    (throw (ex-info msg {:decl decl}))))
                (let [msg (str "unknown type of declaration found: " (first decl))]
                  (throw (ex-info msg {:decl decl})))))
            (recur decls ret)))))))

(defn init-cenv [proto-cenv cname parent interfaces fields ctors methods]
  (let [fields' (into {} (map (fn [[_ name :as field]]
                                (let [modifiers (modifiers-of field)
                                      {:keys [type access]} (parse-modifiers proto-cenv modifiers)]
                                  [(str name) {:type type :access access}])))
                      fields)
        ctors' (reduce (fn [cs [_ _ args :as ctor]]
                         (let [access (access-flags (modifiers-of ctor))
                               arg-types (mapv #(:type (parse-name proto-cenv %)) args)]
                           (conj cs {:access access :arg-types arg-types})))
                       [] ctors)
        methods' (reduce (fn [m [_ name args :as method]]
                           (let [modifiers (modifiers-of method)
                                 {:keys [type access]} (parse-modifiers proto-cenv modifiers
                                                                        :default-type t/VOID)]
                             (update m (str name) (fnil conj [])
                                     {:access access :return-type type
                                      :arg-types (mapv #(:type (parse-name proto-cenv %)) args)})))
                         {} methods)
        class-entry  {:parent parent :interfaces (set interfaces)
                      :fields fields' :ctors ctors' :methods methods'}]
    (assoc-in proto-cenv [:classes cname] class-entry)))

(defn parse-class [[_ cname & body :as class]]
  (let [alias (class-alias cname)
        proto-cenv {:class-name cname :classes {}
                    :aliases (cond-> {} (not= cname alias) (assoc alias cname))}
        {:keys [parent interfaces body]} (parse-supers proto-cenv body)
        {:keys [ctors fields methods]} (parse-class-body cname body)
        parent (or parent t/OBJECT)
        ctors' (if (empty? ctors)
                 [(with-meta `(~'defm ~alias [] (~'super))
                    (select-keys (modifiers-of class) [:public :protected :private]))]
                 ctors)
        cenv (init-cenv proto-cenv cname parent interfaces fields ctors' methods)]
    {:name (str/replace (str cname) \. \/)
     :access (access-flags (modifiers-of class))
     :parent parent
     :interfaces interfaces
     :ctors (mapv (partial parse-method cenv true) ctors')
     :fields (mapv (partial parse-field cenv) fields)
     :methods (mapv (partial parse-method cenv false) methods)}))

(defn parse-unary-op [cenv [_ x] op]
  (let [cenv' (with-context cenv :expression)
        x' (parse-expr cenv' x)
        cs (t/unary-numeric-promotion (:type x'))]
    {:op op
     :context (context-of cenv)
     :type (:type x')
     :operand (apply-conversions cs x')}))

(defn parse-binary-op [cenv [_ x y] op]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' x)
        rhs (parse-expr cenv' y)
        [cl cr] (t/binary-numeric-promotion (:type lhs) (:type rhs))]
    {:op op
     :context (context-of cenv)
     :lhs (apply-conversions cl lhs)
     :rhs (apply-conversions cr rhs)}))

(defn parse-arithmetic [cenv expr op]
  (let [{:keys [lhs] :as ret} (parse-binary-op cenv expr op)]
    (assoc ret :type (:type lhs))))

(defn coerce-to-primitive [cenv [_ x]]
  (let [x' (parse-expr cenv x)]
    (apply-conversions (t/unary-numeric-promotion (:type x')) x')))

(defn fold-binary-op [[op x y & more :as expr]]
  (if more
    (recur (with-meta `(~op (~op ~x ~y) ~@more) (meta expr)))
    expr))

(defmethod parse-expr* '+ [cenv [_ x y & more :as expr]]
  (cond more (parse-expr cenv (fold-binary-op expr))
        y (parse-arithmetic cenv expr :add)
        x (coerce-to-primitive cenv expr)
        :else (parse-expr cenv 0)))

(defmethod parse-expr* '- [cenv [_ x y & more :as expr]]
  (cond more (parse-expr cenv (fold-binary-op expr))
        y (parse-arithmetic cenv expr :sub)
        x (parse-unary-op cenv expr :neg)))

(defmethod parse-expr* '* [cenv [_ x y & more :as expr]]
  (cond more (parse-expr cenv (fold-binary-op expr))
        y (parse-arithmetic cenv expr :mul)
        x (coerce-to-primitive cenv expr)
        :else (parse-expr cenv 1)))

(defmethod parse-expr* '/ [cenv [_ x y & more :as expr]]
  (cond more (parse-expr cenv (fold-binary-op expr))
        y (parse-arithmetic cenv expr :div)
        :else (parse-expr cenv (with-meta `(~'/ 1 ~x) (meta expr)))))

(defmethod parse-expr* '% [cenv expr]
  (parse-arithmetic cenv expr :rem))

(defmethod parse-expr* '& [cenv expr]
  (parse-arithmetic cenv (fold-binary-op expr) :bitwise-and))

(defmethod parse-expr* '| [cenv expr]
  (parse-arithmetic cenv (fold-binary-op expr) :bitwise-or))

(defmethod parse-expr* 'xor [cenv expr]
  (parse-arithmetic cenv (fold-binary-op expr) :bitwise-xor))

(defn parse-shift [cenv [_ x y] op]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' x)
        rhs (parse-expr cenv' y)
        cl (t/unary-numeric-promotion (:type lhs))
        cr (t/unary-numeric-promotion (:type rhs))
        lhs' (apply-conversions cl lhs)
        rhs' (apply-conversions cr rhs)
        rhs' (cond->> rhs'
               (= (:type rhs') t/LONG)
               (apply-conversions [(t/narrowing-primitive-conversion t/LONG t/INT)]))]
    {:op op
     :context (context-of cenv)
     :type (:type lhs')
     :lhs lhs'
     :rhs rhs'}))

(defmethod parse-expr* '<< [cenv expr]
  (parse-shift cenv expr :shift-left))

(defmethod parse-expr* '>> [cenv expr]
  (parse-shift cenv expr :shift-right))

(defmethod parse-expr* '>>> [cenv expr]
  (parse-shift cenv expr :logical-shift-right))

(defn parse-comparison [cenv expr op]
  (if (= (:context cenv) :conditional)
    (let [cenv' (with-context cenv :expression)]
      (-> (parse-binary-op cenv' expr op)
          (assoc :type 'boolean)))
    (parse-expr cenv `(if ~expr true false))))

(defmethod parse-expr* '== [cenv expr]
  (parse-comparison cenv expr :eq))

(defmethod parse-expr* '!= [cenv expr]
  (parse-comparison cenv expr :ne))

(defmethod parse-expr* '< [cenv expr]
  (parse-comparison cenv expr :lt))

(defmethod parse-expr* '> [cenv expr]
  (parse-comparison cenv expr :gt))

(defmethod parse-expr* '<= [cenv expr]
  (parse-comparison cenv expr :le))

(defmethod parse-expr* '>= [cenv expr]
  (parse-comparison cenv expr :ge))

(defn parse-cast [cenv type x]
  (let [x' (parse-expr cenv x)
        cs (t/casting-conversion cenv (:type x') type)]
    (apply-conversions cs x')))

(defmethod parse-expr* 'byte [cenv [_ x]]
  (parse-cast cenv t/BYTE x))

(defmethod parse-expr* 'char [cenv [_ x]]
  (parse-cast cenv t/CHAR x))

(defmethod parse-expr* 'short [cenv [_ x]]
  (parse-cast cenv t/SHORT x))

(defmethod parse-expr* 'int [cenv [_ x]]
  (parse-cast cenv t/INT x))

(defmethod parse-expr* 'long [cenv [_ x]]
  (parse-cast cenv t/LONG x))

(defmethod parse-expr* 'float [cenv [_ x]]
  (parse-cast cenv t/FLOAT x))

(defmethod parse-expr* 'double [cenv [_ x]]
  (parse-cast cenv t/DOUBLE x))

(defmethod parse-expr* 'cast [cenv [_ t x]]
  (let [t' (t/tag->type cenv t)]
    (parse-cast cenv t' x)))

(defmethod parse-expr* 'instance? [cenv [_ c x]]
  {:op :instance?
   :context (context-of cenv)
   :type t/BOOLEAN
   :class (t/tag->type cenv c)
   :operand (parse-expr (with-context cenv :expression) x)})

(defmethod parse-expr* 'do [cenv [_ & body]]
  (parse-exprs cenv body))

(defmethod parse-expr* 'let* [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (parse-bindings cenv bindings)
        body' (parse-exprs cenv' body)]
    {:op :let :type (:type body')
     :bindings bindings'
     :body body'}))

(defmethod parse-expr* 'let [cenv expr]
  (parse-expr cenv (with-meta `(let* ~@(rest expr)) (meta expr))))

(defmethod parse-expr* 'set! [cenv [_ target expr]]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' target)
        rhs (parse-expr cenv' expr)
        cs (t/assignment-conversion cenv (:type rhs) (:type lhs))
        rhs' (apply-conversions cs rhs)]
    (case (:op lhs)
      :field-access
      (cond-> {:op :field-update
               :context (context-of cenv)
               :type (:type lhs)
               :class (:class lhs)
               :name (:name lhs)
               :rhs rhs'}
        (:target lhs) (assoc :target (:target lhs)))

      :array-access
      {:op :array-update
       :context (context-of cenv)
       :type (:type lhs)
       :array (:array lhs)
       :index (:index lhs)
       :expr rhs'}

      {:op :assignment
       :context (context-of cenv)
       :type (:type lhs)
       :lhs lhs
       :rhs rhs'})))

(defmethod parse-expr* 'inc! [cenv [_ target by]]
  (let [by (or by 1)
        target' (parse-expr (with-context cenv :expression) target)]
    (if (and (= (:op target') :local)
             (when-let [{:keys [to]} (t/widening-primitive-conversion (:type target') t/INT)]
               (= to t/INT))
             (<= 0 by Byte/MAX_VALUE))
      (-> {:op :increment, :target target', :type (:type target'), :by by}
          (inherit-context cenv))
      (parse-expr cenv `(set! ~target (~'+ ~target ~by))))))

(defmethod parse-expr* 'dec! [cenv [_ target by]]
  (let [by (or by 1)
        target' (parse-expr (with-context cenv :expression) target)]
    (if (and (= (:op target') :local)
             (when-let [{:keys [to]} (t/widening-primitive-conversion (:type target') t/INT)]
               (= to t/INT))
             (<= 0 by (- Byte/MIN_VALUE)))
      (-> {:op :increment, :target target', :type (:type target'), :by (- by)}
          (inherit-context cenv))
      (parse-expr cenv `(set! ~target (~'- ~target ~by))))))

(defn unbox-if-possible [x]
  (if-let [unbox (t/unboxing-conversion (:type x))]
    (apply-conversions [unbox] x)
    x))

(defmethod parse-expr* 'if [cenv [_ test then else]]
  (cond (true? test) (parse-expr cenv then)
        (false? test) (parse-expr cenv else)
        :else
        (let [test' (-> (parse-expr (with-context cenv :conditional) test)
                        unbox-if-possible)
              statement? (= (context-of cenv) :statement)
              cenv' (cond-> cenv (not statement?) (assoc :context :expression))
              then' (parse-expr cenv' then)
              else' (some->> else (parse-expr cenv'))]
          (cond-> (inherit-context {:op :if, :test test', :then then'} cenv)
            else' (assoc :else else')
            (not statement?) (assoc :type (:type then'))))))

(defn extract-label [expr]
  (:label (meta expr)))

(defmethod parse-expr* 'while [cenv [_ cond & body :as expr]]
  (let [label (extract-label expr)]
    (cond-> {:op :while
             :context (context-of cenv)
             :cond (-> (parse-expr (with-context cenv :conditional) cond)
                       unbox-if-possible)
             :body (parse-exprs (with-context cenv :statement) body)}
      label (assoc :label label))))

(defmethod parse-expr* 'for [cenv [_ args & body :as form]]
  (if (= (count args) 2)
    ;; Enhanced for-loop
    (let [[lname expr] args
          expr' (parse-expr cenv expr)
          form' (if (t/array-type? (:type expr'))
                  `(let* [array# ~expr
                          len# (.-length array#)
                          ~lname (~'aget array# 0)]
                     (~'for [i# 0 (~'< i# len#) (~'inc! i#)]
                      (set! ~lname (~'aget array# i#))
                      ~@body))
                  `(~'for [i# (.iterator ~expr) (.hasNext i#) nil]
                    (let* [~lname (.next i#)]
                      ~@body)))]
      (parse-expr cenv (with-meta form' (meta form))))
    (let [[lname init cond step] args
          [cenv' bindings'] (parse-bindings cenv [lname init])
          label (extract-label form)]
      {:op :let
       :bindings bindings'
       :body
       (cond-> {:op :for
                :context (context-of cenv)
                :cond (-> (parse-expr (with-context cenv' :conditional) cond)
                          unbox-if-possible)
                :step (parse-expr (with-context cenv' :statement) step)
                :body (parse-exprs (with-context cenv' :statement) body)}
         label (assoc :label label))})))

(defmethod parse-expr* 'continue [_ [_ label]]
  (cond-> {:op :continue}
    label (assoc :label label)))

(defmethod parse-expr* 'break [_ [_ label]]
  (cond-> {:op :break}
    label (assoc :label label)))

(defmethod parse-expr* 'new [cenv [_ type & args]]
  (let [type' (t/tag->type cenv type)]
    (if (t/array-type? type')
      (if (vector? (first args))
        (let [elems (first args)
              arr (gensym)]
          (parse-expr cenv `(~'let [~arr (new ~type ~(count elems))]
                             ~@(for [[i init] (map-indexed vector elems)]
                                 `(~'aset ~arr ~i ~init))
                             ~arr)))
        {:op :new-array
         :context (context-of cenv)
         :type type'
         :length (parse-expr (with-context cenv :expression) (first args))})
      (let [cenv' (with-context cenv :expression)
            args' (map (partial parse-expr cenv') args)
            ctor (t/find-ctor cenv type' (map :type args'))]
        {:op :new
         :context (context-of cenv)
         :type type'
         :access (:access ctor)
         :arg-types (:arg-types ctor)
         :args args'}))))

(defmethod parse-expr* '. [cenv [_ target property & maybe-args :as expr]]
  (if (and (seq? property) (nil? maybe-args))
    (parse-expr cenv `(. ~target ~@property))
    (let [cenv' (with-context cenv :expression)
          target' (when (or (not (symbol? target))
                            (namespace target)
                            (find-lname cenv target))
                    (parse-expr cenv' target))
          target-type (or (:type target') (t/tag->type cenv target))
          pname (name property)]
      (if (str/starts-with? pname "-")
        (if (and (t/array-type? target-type) (= pname "-length"))
          {:op :array-length
           :context (context-of cenv)
           :type t/INT
           :array target'}
          (let [pname' (subs pname 1)
                field (t/find-field cenv target-type pname')]
            (cond-> {:op :field-access
                     :context (context-of cenv)
                     :type (:type field)
                     :class (:class field)
                     :name pname'}
              target' (assoc :target target'))))
        (let [args' (map (partial parse-expr cenv') maybe-args)
              method (t/find-method cenv target-type pname (map :type args'))]
          (cond-> {:op :method-invocation
                   :context (context-of cenv)
                   :interface? (:interface? method false)
                   :type (:return-type method)
                   :access (:access method)
                   :arg-types (:arg-types method)
                   :class (:class method)
                   :name pname
                   :args args'}
            target' (assoc :target target')))))))

(defn fold-aget [[_ arr index & indices :as expr]]
  (if (empty? indices)
    expr
    (recur (with-meta `(~'aget (~'aget ~arr ~index) ~@indices) (meta expr)))))

(defmethod parse-expr* 'aget [cenv [_ arr index & indices :as expr]]
  (if indices
    (parse-expr cenv (fold-aget expr))
    (let [cenv' (with-context cenv :expression)
          arr (parse-expr cenv' arr)]
      {:op :array-access
       :context (context-of cenv)
       :type (t/element-type (:type arr))
       :array arr
       :index (parse-expr cenv' index)})))

(defmethod parse-expr* 'aset [cenv [_ arr index & more :as expr]]
  (if (next more)
    (let [indices (cons index (butlast more))
          form (with-meta
                 `(~'aset (~'aget ~arr ~@(butlast indices)) ~(last indices) ~(last more))
                 (meta expr))]
      (parse-expr cenv form))
    (let [cenv' (with-context cenv :expression)
          arr (parse-expr cenv' arr)]
      {:op :array-update
       :context (context-of cenv)
       :type (t/element-type (:type arr))
       :array arr
       :index (parse-expr cenv' index)
       :expr (parse-expr cenv' (first more))})))
