(ns jise.parse
  (:refer-clojure :exclude [find-var])
  (:require [clojure.string :as str]
            [jise.misc :as misc]
            [jise.type :as t])
  (:import [clojure.asm Type]
           [clojure.java.api Clojure]))

(defn modifiers-of [[_ name :as form]]
  (merge (meta form) (meta name)))

(defn access-flags [modifiers]
  (-> modifiers
      (select-keys [:abstract :static :public :protected :private :final :transient :volatile])
      keys
      set))

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
  (if (:conditional context)
    (-> context (conj :expression) (disj :conditional))
    context))

(defn with-context [x context]
  (assoc x :context #{context}))

(defn inherit-context [x y & {:keys [return?]}]
  (assoc x :context
         (cond-> (context-of y)
           (not (nil? return?)) ((if return? conj disj) :return))))

(defn apply-conversions [conversions src]
  (reduce (fn [src {:keys [conversion to]}]
            (-> {:op conversion
                 :type to
                 :src (with-context src :expression)}
                (inherit-context src)))
          src
          conversions))

(defn find-in-current-class [cenv & ks]
  (get-in cenv (into [:classes (:class-name cenv)] ks)))

(defn find-lname [cenv sym]
  (get (:lenv cenv) (name sym)))

(defn find-var [cenv sym]
  (when-let [var (resolve sym)]
    (let [vname (symbol (subs (str var) 2))]
      (-> (swap! (:vars cenv)
                 (fn [{:keys [var->entry fields] :as vars}]
                   (if (get var->entry vname)
                     vars
                     (let [field-name (gensym (:name (meta var)))
                           entry {:var var :var-name vname :field-name field-name}]
                       (-> vars
                           (assoc-in [:var->entry vname] entry)
                           (update :fields conj (name field-name)))))))
          (get-in [:var->entry vname])))))

(def VAR_TYPE (Type/getType clojure.lang.Var))

(defn find-field [cenv class fname]
  (or (t/find-field cenv class fname)
      (when (and (= class (:class-type cenv))
                 (get-in @(:vars cenv) [:fields fname]))
        {:class class :type VAR_TYPE :access #{:public :static}})))

(declare parse-expr)

(defmulti parse-expr* (fn [cenv expr] (misc/fixup-ns (first expr))))
(defmethod parse-expr* :default [cenv expr]
  (let [expanded (misc/macroexpand cenv expr)]
    (if-not (identical? expanded expr)
      (parse-expr cenv expanded)
      (throw (ex-info (str "unsupported expr: " (pr-str expr)) {:expr expr})))))

(defn parse-symbol [cenv sym]
  (if-let [tag (:tag (meta sym))]
    (parse-expr cenv `(~'cast ~tag ~(vary-meta sym dissoc :tag)))
    (letfn [(parse-as-field [cenv target]
              (parse-expr cenv (with-meta `(. ~target ~(symbol (str \- (name sym)))) (meta sym))))]
      (if-let [cname (namespace sym)]
        (if-let [{:keys [field-name]} (find-var cenv sym)]
          (let [form `(.deref (. ~(:class-name cenv) ~(symbol (str \- field-name))))]
            (parse-expr cenv (with-meta form (meta sym))))
          (parse-as-field cenv (symbol cname)))
        (if-let [{:keys [index type foreign?]} (find-lname cenv sym)]
          (if foreign?
            (parse-as-field cenv 'this)
            (inherit-context {:op :local :index index :type type} cenv))
          (if-let [f (find-field cenv (:class-type cenv) (name sym))]
            (let [target (if (:static (:access f)) (:class-name cenv) 'this)]
              (parse-as-field cenv target))
            (throw (ex-info (str "unknown variable found: " sym) {:variable sym}))))))))

(defn parse-ctor-invocation [cenv [op & args]]
  (let [cenv' (with-context cenv :expression)
        args' (map (partial parse-expr cenv') args)
        class (if (= op 'this)
                (:class-type cenv)
                (find-in-current-class cenv :parent))
        ctor (t/find-ctor cenv class (map :type args'))
        initializer (when (= op 'super) (find-in-current-class cenv :initializer))]
    (-> {:op :ctor-invocation
         :class class
         :access (:access ctor)
         :param-types (:param-types ctor)
         :args args'}
        (inherit-context (cond-> cenv initializer (with-context :statement)))
        (cond-> initializer (assoc :initializer (parse-expr cenv initializer))))))

(defn parse-sugar [cenv [op :as expr]]
  (or (when-let [maybe-array (or (find-lname cenv op)
                                 (find-field cenv (:class-type cenv) (str op)))]
        (when (t/array-type? (:type maybe-array))
          (parse-expr cenv (with-meta `(~'aget ~@expr) (meta expr)))))
      (when-let [{:keys [var field-name]} (and (namespace op) (find-var cenv op))]
        (when-not (:macro (meta var))
          (let [form `(.invoke (. ~(:class-name cenv) ~(symbol (str \- field-name)))
                               ~@(map (fn [x] `(~'cast Object ~x)) (rest expr)))]
            (parse-expr cenv (with-meta form (meta expr))))))))

(defn parse-seq [cenv expr]
  (if-let [tag (:tag (meta expr))]
    (parse-expr cenv `(~'cast ~tag ~(vary-meta expr dissoc :tag)))
    (let [{:keys [tag line label]} (meta expr)
          cenv' (if label (inherit-context cenv cenv :return? false) cenv)
          expr' (let [op (first expr)]
                  (if ('#{this super} op)
                    (parse-ctor-invocation cenv' expr)
                    (and (symbol? op)
                         (or (parse-sugar cenv' expr)
                             (parse-expr* cenv' expr)))))]
      (as-> expr' expr'
        (if line
          (assoc expr' :line line)
          expr')
        (if label
          (if (or (not= (:op expr') :labeled)
                  (not= (:label expr') label))
            (-> {:op :labeled :label label :target expr'}
                (inherit-context cenv))
            (inherit-context expr' cenv))
          expr')))))

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
    (or (when (and (:return (:context expr'))
                   (not= (or (:type expr') t/VOID) (:return-type cenv)))
          (let [expr' (if (= (:type expr') t/VOID)
                        ;; insert implicit (do ... nil)
                        (-> {:op :do
                             :type nil
                             :exprs [(with-context expr' :statement)
                                     (parse-literal cenv nil)]}
                            (inherit-context cenv :return? false))
                        expr')]
            (when-let [cs (seq (t/assignment-conversion cenv (:type expr') (:return-type cenv)))]
              (apply-conversions cs expr'))))
        expr')))

(defn  parse-exprs [cenv body]
  (let [cenv' (with-context cenv :statement)
        exprs' (mapv parse-expr (repeat cenv') (butlast body))
        last' (parse-expr cenv (last body))]
    (-> {:op :do :type (:type last')
         :exprs (conj exprs' last')}
        (inherit-context cenv :return? false))))

(defn parse-name [proto-cenv name & {:keys [default-type]}]
  (let [{:keys [access type]} (parse-modifiers proto-cenv (meta name) :default-type default-type)]
    {:name name
     :type type
     :access access}))

(defn next-index [cenv type]
  (first (swap-vals! (:next-index cenv) + (t/type-category type))))

(defn parse-binding [cenv lname init]
  (let [init' (some->> init (parse-expr (with-context cenv :expression)))
        lname' (parse-name cenv lname :default-type (:type init'))
        init' (if-let [cs (and init' (t/casting-conversion cenv (:type init') (:type lname')))]
                (apply-conversions cs init')
                init')]
    (-> lname'
        (update :name name)
        (assoc :index (next-index cenv (:type lname')))
        (cond-> init' (assoc :init init')))))

(defn parse-bindings [cenv bindings]
  (loop [[lname init & bindings] bindings
         cenv' (with-context cenv :expression)
         ret []]
    (if lname
      (let [b (parse-binding cenv' lname init)
            cenv' (assoc-in cenv' [:lenv (:name b)] b)]
        (recur bindings cenv' (conj ret b)))
      [(inherit-context cenv' cenv) ret])))

(defn parse-method [cenv ctor? [_ mname args & body :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access type]} (parse-modifiers cenv modifiers :default-type t/VOID)
        init-lenv (if (:static access)
                    {}
                    {"this" {:index 0 :type (:class-type cenv)}})
        init-index (count init-lenv)
        [cenv' args'] (parse-bindings (assoc cenv
                                             :lenv (merge init-lenv (:enclosing-env cenv))
                                             :next-index (atom init-index))
                                      (interleave args (repeat nil)))
        return-type (if ctor? t/VOID type)
        context (if (= return-type t/VOID) :statement :expression)]
    (cond-> {:return-type return-type
             :args args'
             :access access
             :body (when-not (:abstract access)
                     (parse-exprs (-> cenv'
                                      (assoc :return-type return-type
                                             :context #{context :tail :return}))
                                  body))}
      ctor? (assoc :ctor? ctor?)
      (not ctor?) (assoc :name (str mname)))))

(defn parse-supers [proto-cenv [maybe-supers & body]]
  (let [supers (when (vector? maybe-supers) maybe-supers)
        supers' (map (partial t/tag->type proto-cenv) supers)
        {[parent] false
         interfaces true} (group-by #(.isInterface (t/type->class %)) supers')]
    {:parent parent
     :interfaces interfaces
     :body (cond->> body (nil? supers) (cons maybe-supers))}))

(defn class-alias [cname]
  (symbol (str/replace cname #"^.*\.([^.]+)" "$1")))

(defn convert-def-to-set [alias [_ name init :as def]]
  (when init
    (let [static? (:static (modifiers-of def))]
      (cond-> `(set! (. ~(if static? alias 'this)
                        ~(symbol (str \- name)))
                     ~init)
        static? (with-meta {:static true})))))

(defn parse-class-body [cname body]
  (let [alias (class-alias cname)]
    (loop [decls body
           ret {:ctors [], :fields [], :methods [], :initializer []}]
      (if (empty? decls)
        ret
        (let [[decl & decls] decls]
          (if (seq? decl)
            (case (misc/symbol-without-jise-ns (first decl))
              def (let [[_ name init] decl
                        decl' (convert-def-to-set alias decl)
                        ret' (-> ret
                                 (update :fields conj decl)
                                 (cond-> decl' (update :initializer conj decl')))]
                    (recur decls ret'))
              defm (let [[_ name] decl]
                     (recur decls
                            (if (= name alias)
                              (update ret :ctors conj decl)
                              (update ret :methods conj decl))))
              do (recur (concat (rest decl) decls) ret)
              (let [v (resolve (first decl))
                    [decls ret] (if (and v (:macro (meta v)))
                                  [(cons (misc/macroexpand {} decl) decls) ret]
                                  [decls (update ret :initializer conj decl)])]
                (recur decls ret)))
            (recur decls ret)))))))

(defn init-cenv [proto-cenv cname parent interfaces fields ctors methods initializer]
  (let [fields' (into {} (map (fn [[_ name :as field]]
                                (let [modifiers (modifiers-of field)
                                      {:keys [type access]} (parse-modifiers proto-cenv modifiers)]
                                  [(str name) {:type type :access access}])))
                      fields)
        ctors' (reduce (fn [cs [_ _ args :as ctor]]
                         (let [access (access-flags (modifiers-of ctor))
                               param-types (mapv #(:type (parse-name proto-cenv %)) args)]
                           (conj cs {:access access :param-types param-types})))
                       [] ctors)
        methods' (reduce (fn [m [_ name args :as method]]
                           (let [modifiers (modifiers-of method)
                                 {:keys [type access]} (parse-modifiers proto-cenv modifiers
                                                                        :default-type t/VOID)]
                             (update m (str name) (fnil conj [])
                                     {:access access :return-type type
                                      :param-types (mapv #(:type (parse-name proto-cenv %)) args)})))
                         {} methods)
        class-entry  {:parent parent :interfaces (set interfaces) :initializer initializer
                      :fields fields' :ctors ctors' :methods methods'}]
    (assoc-in proto-cenv [:classes cname] class-entry)))

(defn filter-static [exprs]
  (reduce (fn [ret expr]
            (let [k (if (:static (modifiers-of expr))
                      :static
                      :non-static)]
              (update ret k conj expr)))
          {:static [] :non-static []}
          exprs))

(defn synthesize-fields [{:keys [enclosing-env vars]}]
  (as-> [] fields
    (reduce (fn [fields [name {:keys [type used?]}]]
              (if @used?
                (let [type' (t/type->tag type)]
                  (conj fields `(def ~(with-meta (symbol name) {:tag type' :public true}))))
                fields))
            fields
            enclosing-env)
    (reduce (fn [fields [_ {:keys [field-name var-name]}]]
              (conj fields `(def ~(with-meta field-name {:tag 'clojure.lang.Var :private true :static true})
                              (~'cast clojure.lang.Var (Clojure/var (~'cast Object ~(str var-name)))))))
            fields
            (:var->entry @vars))))

(defn parse-class [enclosing-env [_ cname & body :as class]]
  (let [alias (class-alias cname)
        proto-cenv {:class-name cname :classes {}
                    :aliases (cond-> {} (not= cname alias) (assoc alias cname))}
        {:keys [parent interfaces body]} (parse-supers proto-cenv body)
        {:keys [ctors fields methods initializer]} (parse-class-body cname body)
        {initializer :non-static static-initializer :static} (filter-static initializer)
        parent (or parent t/OBJECT)
        ctors' (if (empty? ctors)
                 [(with-meta `(~'defm ~alias [] (~'super))
                    (select-keys (modifiers-of class) [:public :protected :private]))]
                 ctors)
        cenv (-> proto-cenv
                 (init-cenv cname parent interfaces fields ctors' methods
                            (when (seq initializer) `(do ~@initializer)))
                 (assoc :enclosing-env enclosing-env
                        :vars (atom {:var->entry {} :fields #{}}))
                 (as-> cenv (assoc cenv :class-type (t/tag->type cenv cname))))
        ctors' (mapv (partial parse-method cenv true) ctors')
        methods' (mapv (partial parse-method cenv false) methods)
        synthesized-fields (synthesize-fields cenv)]
    {:name (str/replace (str cname) \. \/)
     :access (access-flags (modifiers-of class))
     :parent parent
     :interfaces interfaces
     :static-initializer (when-let [init (->> (:static (filter-static synthesized-fields))
                                              (keep (partial convert-def-to-set alias))
                                              (concat static-initializer)
                                              seq)]
                           (let [m `^:static (~'defm ~'<clinit> [] ~@init)]
                             (-> (parse-method cenv false m)
                                 (assoc :static-initializer? true))))
     :ctors ctors'
     :methods methods'
     :fields (mapv (partial parse-field cenv)
                   (concat fields synthesized-fields))}))

(defn parse-unary-op [cenv [_ x] op]
  (let [cenv' (with-context cenv :expression)
        x' (parse-expr cenv' x)
        cs (t/unary-numeric-promotion (:type x'))]
    (-> {:op op
         :type (:type x')
         :operand (apply-conversions cs x')}
        (inherit-context cenv))))

(defn parse-binary-op
  ([cenv [_ x y] op]
   (let [cenv' (with-context cenv :expression)
         lhs (parse-expr cenv' x)
         rhs (parse-expr cenv' y)]
     (parse-binary-op cenv lhs rhs op)))
  ([cenv lhs rhs op]
   (let [[cl cr] (t/binary-numeric-promotion (:type lhs) (:type rhs))]
     (-> {:op op
          :lhs (apply-conversions cl lhs)
          :rhs (apply-conversions cr rhs)}
         (inherit-context cenv)))))

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

(defmethod parse-expr* '! [cenv [_ operand :as expr]]
  (parse-expr cenv (with-meta `(~'xor ~operand -1) (meta expr))))

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
    (-> {:op op
         :type (:type lhs')
         :lhs lhs'
         :rhs rhs'}
        (inherit-context cenv))))

(defmethod parse-expr* '<< [cenv expr]
  (parse-shift cenv expr :shift-left))

(defmethod parse-expr* '>> [cenv expr]
  (parse-shift cenv expr :shift-right))

(defmethod parse-expr* '>>> [cenv expr]
  (parse-shift cenv expr :logical-shift-right))

(defn fold-comparison [[op & args :as expr]]
  (with-meta
    `(~'and ~@(map (fn [[x y]] `(~op ~x ~y)) (partition 2 1 args)))
    (meta expr)))

(defn parse-comparison
  ([cenv [_ x y & more :as expr] op]
   (if (:conditional (:context cenv))
     (if more
       (parse-expr cenv (fold-comparison expr))
       (let [cenv' (with-context cenv :expression)
             x' (parse-expr cenv' x)
             y' (parse-expr cenv' y)]
         (parse-comparison cenv' x' y' op)))
     (parse-expr cenv `(if ~expr true false))))
  ([cenv lhs rhs op]
   (-> (parse-binary-op cenv lhs rhs op)
       (assoc :type t/BOOLEAN))))

(defn parse-cmp-0 [cenv x op default-op default]
  (if (= x 0)
    (parse-expr cenv default)
    (let [cenv' (with-context cenv :expression)
          x' (parse-expr cenv' x)]
      (if (= (:type x') t/INT)
        {:op op
         :type t/BOOLEAN
         :operand x'}
        (parse-comparison cenv' x' (parse-literal cenv' 0) default-op)))))

(defn parse-eq-null [cenv x op]
  (if (nil? x)
    (parse-expr cenv true)
    (let [x' (parse-expr (with-context cenv :expression) x)]
      {:op op
       :type t/BOOLEAN
       :operand x'})))

(defmethod parse-expr* '== [cenv [_ x y & more :as expr]]
  (or (when-not more
        (cond (or (= x 0) (= y 0)) (parse-cmp-0 cenv (if (= x 0) y x) :eq-0 :eq true)
              (or (nil? x) (nil? y)) (parse-eq-null cenv (if (nil? x) y x) :eq-null)))
      (parse-comparison cenv expr :eq)))

(defmethod parse-expr* '!= [cenv [_ x y & more :as expr]]
  (or (when-not more
        (cond (or (= x 0) (= y 0)) (parse-cmp-0 cenv (if (= x 0) y x) :ne-0 :ne false)
              (or (nil? x) (nil? y)) (parse-eq-null cenv (if (nil? x) y x) :ne-null)))
      (parse-comparison cenv expr :ne)))

(defmethod parse-expr* '< [cenv [_ x y & more :as expr]]
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv (if (= x 0) y x) :lt-0 :lt false)
    (parse-comparison cenv expr :lt)))

(defmethod parse-expr* '> [cenv [_ x y & more :as expr]]
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv (if (= x 0) y x) :gt-0 :gt false)
    (parse-comparison cenv expr :gt)))

(defmethod parse-expr* '<= [cenv [_ x y & more :as expr]]
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv (if (= x 0) y x) :le-0 :le true)
    (parse-comparison cenv expr :le)))

(defmethod parse-expr* '>= [cenv [_ x y & more :as expr]]
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv (if (= x 0) y x) :ge-0 :ge true)
    (parse-comparison cenv expr :ge)))

(defmethod parse-expr* 'and [cenv [_ & exprs :as expr]]
  (if (:conditional (:context cenv))
    (case (count exprs)
      0 (parse-expr cenv true)
      1 (parse-expr cenv (first exprs))
      {:op :and
       :type t/BOOLEAN
       :exprs (mapv (partial parse-expr cenv) exprs)})
    (parse-expr cenv `(if ~expr true false))))

(defn negate-expr [{:keys [op] :as expr}]
  (case op
    :not (:expr expr)
    :and {:op :or
          :type t/BOOLEAN
          :exprs (butlast (:exprs expr))
          :expr (negate-expr (last (:exprs expr)))}
    :or {:op :and
         :type t/BOOLEAN
         :exprs (conj (mapv negate-expr (:exprs expr)) (:expr expr))}
    {:op :not
     :type t/BOOLEAN
     :expr expr}))

(defmethod parse-expr* 'or [cenv [_ & exprs :as expr]]
  (if (:conditional (:context cenv))
    (case (count exprs)
      0 (parse-expr cenv false)
      1 (parse-expr cenv (first exprs))
      {:op :or
       :type t/BOOLEAN
       :exprs (mapv #(negate-expr (parse-expr cenv %)) (butlast exprs))
       :expr (parse-expr cenv (last exprs))})
    (parse-expr cenv `(if ~expr true false))))

(defmethod parse-expr* 'not [cenv [_ operand :as expr]]
  (if (:conditional (:context cenv))
    (negate-expr (parse-expr cenv operand))
    (parse-expr cenv `(if ~expr true false))))

(defn parse-cast [cenv type x]
  (let [x' (parse-expr cenv x)
        cs (t/casting-conversion cenv (:type x') type)]
    (apply-conversions cs x')))

(defmethod parse-expr* 'boolean [cenv [_ x]]
  (parse-cast cenv t/BOOLEAN x))

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

(defmethod parse-expr* '= [cenv [_ x y :as expr]]
  ;; FIXME: Remove explicit cast here once we could invoke "loosely" matching method
  (parse-expr cenv (with-meta `(.equals ~x (~'cast Object ~y)) (meta expr))))

(defmethod parse-expr* 'nil? [cenv [_ arg :as expr]]
  (parse-expr cenv (with-meta `(~'== ~arg nil) (meta expr))))

(defmethod parse-expr* `nil? [cenv [_ arg :as expr]]
  (parse-expr cenv (with-meta `(~'== ~arg nil) (meta expr))))

(defmethod parse-expr* 'str [cenv [_ & args :as expr]]
  (if (every? string? args)
    (parse-expr cenv (apply str args))
    (let [form `(-> (new StringBuilder)
                    ~@(map (fn [arg] `(.append ~arg)) args)
                    .toString)]
      (parse-expr cenv (with-meta form (meta expr))))))

(defmethod parse-expr* 'instance? [cenv [_ c x]]
  (-> {:op :instance?
       :type t/BOOLEAN
       :class (t/tag->type cenv c)
       :operand (parse-expr (with-context cenv :expression) x)}
      (inherit-context cenv)))

(defmethod parse-expr* 'do [cenv [_ & body]]
  (parse-exprs cenv body))

(defmethod parse-expr* 'let* [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (parse-bindings cenv bindings)
        body' (parse-exprs cenv' body)]
    (-> {:op :let :type (:type body')
         :bindings bindings'
         :body body'}
        (inherit-context cenv :return? false))))

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
      (-> {:op :field-update
           :type (:type lhs)
           :class (:class lhs)
           :name (:name lhs)
           :rhs rhs'}
          (inherit-context cenv)
          (cond-> (:target lhs) (assoc :target (:target lhs))))

      :array-access
      (-> {:op :array-update
           :type (:type lhs)
           :array (:array lhs)
           :index (:index lhs)
           :expr rhs'}
          (inherit-context cenv))

      (-> {:op :assignment
           :type (:type lhs)
           :lhs lhs
           :rhs rhs'}
          (inherit-context cenv)))))

(defmethod parse-expr* 'inc! [cenv [_ target by]]
  (let [by (or by 1)
        {:keys [type] :as target'} (parse-expr (with-context cenv :expression) target)]
    (if (and (= (:op target') :local)
             (or (= type t/INT)
                 (when-let [{:keys [to]} (t/widening-primitive-conversion type t/INT)]
                   (= to t/INT)))
             (<= 0 by Byte/MAX_VALUE))
      (-> {:op :increment, :target target', :type type, :by by}
          (inherit-context cenv))
      (parse-expr cenv `(set! ~target (~'+ ~target ~by))))))

(defmethod parse-expr* 'dec! [cenv [_ target by]]
  (let [by (or by 1)
        {:keys [type] :as target'} (parse-expr (with-context cenv :expression) target)]
    (if (and (= (:op target') :local)
             (or (= type t/INT)
                 (when-let [{:keys [to]} (t/widening-primitive-conversion type t/INT)]
                   (= to t/INT)))
             (<= 0 by (- Byte/MIN_VALUE)))
      (-> {:op :increment, :target target', :type type, :by (- by)}
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
        (let [test' (parse-expr (with-context cenv :conditional) test)
              test' (if-let [cs (t/casting-conversion cenv (:type test') t/BOOLEAN)]
                      (apply-conversions cs test')
                      test')
              cenv' (if (and (:tail (:context cenv)) (nil? else))
                      (with-context cenv :statement)
                      cenv)
              then' (parse-expr cenv' then)
              else' (some->> else (parse-expr cenv'))
              node {:op :if, :type (:type then'), :test test', :then then'}]
          (if else'
            (-> node
                (assoc :else else')
                (inherit-context cenv :return? false))
            (inherit-context node cenv)))))

(defn parse-case-clause [cenv sym [ks expr]]
  (let [ks (if (seq? ks) (vec ks) [ks])
        str? (string? (first ks))
        ks' (cond->> ks str? (mapv #(.hashCode ^String %)))
        guard (when str?
                (->> (if (> (count ks) 1)
                       `(~'or ~@(map (fn [k] `(~'= ~sym ~k)) ks))
                       `(~'= ~sym ~(first ks)))
                     (parse-expr (with-context cenv :conditional))))
        expr' (parse-expr cenv expr)]
    (cond-> {:keys ks' :type (:type expr') :body expr'}
      guard (assoc :guard guard))))

(defmethod parse-expr* 'case [cenv [_ test & clauses :as expr]]
  (if-let [l (find-lname cenv test)]
    (let [default (when (odd? (count clauses))
                    (last clauses))
          cenv' (if (and (:tail (:context cenv)) (nil? default))
                  (with-context cenv :statement)
                  cenv)
          clauses' (->> (partition 2 clauses)
                        (mapv (partial parse-case-clause cenv' test)))
          default' (some->> default (parse-expr cenv'))
          node {:op :switch
                :type (:type (or (first clauses') default'))
                :test (let [cenv' (with-context cenv :expression)]
                        (if (= (:type l) t/STRING)
                          (parse-expr cenv' `(.hashCode ~test))
                          (parse-expr cenv' test)))
                :clauses clauses'}]
      (if default'
        (-> node
            (assoc :default default')
            (inherit-context cenv :return? false))
        (inherit-context node cenv)))
    (let [h (gensym 'h)
          form `(let* [~h ~test]
                  ~(with-meta
                     `(~'case ~h ~@clauses)
                     (meta expr)))]
      (parse-expr cenv form))))

(defn extract-label [expr]
  (:label (meta expr)))

(defmethod parse-expr* 'while [cenv [_ cond & body :as expr]]
  (let [label (extract-label expr)
        cond' (parse-expr (with-context cenv :conditional) cond)
        cond' (if-let [cs (t/casting-conversion cenv (:type cond') t/BOOLEAN)]
                (apply-conversions cs cond')
                cond')]
    (-> {:op :while
         :cond cond'
         :body (parse-exprs (with-context cenv :statement) body)}
        (inherit-context cenv)
        (cond-> label (assoc :label label)))))

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
          cond' (parse-expr (with-context cenv' :conditional) cond)
          cond' (if-let [cs (t/casting-conversion cenv' (:type cond') t/BOOLEAN)]
                  (apply-conversions cs cond')
                  cond')
          label (extract-label form)]
      (-> {:op :let
           :bindings bindings'
           :body
           (-> {:op :for
                :cond cond'
                :step (parse-expr (with-context cenv' :statement) step)
                :body (parse-exprs (with-context cenv' :statement) body)}
               (inherit-context cenv)
               (cond-> label (assoc :label label)))}
          (inherit-context cenv :return? false)))))

(defn seq-prefixed-with? [prefix x]
  (and (seq? x) (= (first x) prefix)))

(defn append-finally [cenv finally-clause exprs]
  (if finally-clause
    (if (:expression (context-of cenv))
      `(let* [v# (do ~@exprs)] ~@finally-clause v#)
      `(do ~@exprs ~@finally-clause))
    `(do ~@exprs)))

(defn parse-catch-clause [cenv finally-clause [_ class lname & body]]
  (let [class' (t/tag->type cenv class)
        [cenv' [b]] (parse-bindings cenv [(with-meta lname {:tag class}) nil])
        body' (parse-expr cenv' (append-finally cenv finally-clause body))]
    {:class class'
     :type (:type body')
     :index (:index b)
     :body body'}))

(defmethod parse-expr* 'try [cenv [_ & body]]
  (let [[body clauses] (split-with #(and (not (seq-prefixed-with? 'catch %))
                                         (not (seq-prefixed-with? 'finally %)))
                                   body)
        [catch-clauses finally-clauses] (split-with #(not (seq-prefixed-with? 'finally %)) clauses)
        finally-clause (nfirst finally-clauses)
        append-finally (partial append-finally cenv finally-clause)
        body' (parse-expr cenv (append-finally body))]
    (-> {:op :try
         :type (:type body')
         :body body'
         :catch-clauses (mapv (partial parse-catch-clause cenv finally-clause)
                              catch-clauses)}
        (inherit-context cenv :return? false))))

(defmethod parse-expr* 'continue [cenv [_ label]]
  (-> {:op :continue}
      (inherit-context cenv :return? false)
      (cond-> label (assoc :label label))))

(defmethod parse-expr* 'break [cenv [_ label]]
  (-> {:op :break}
      (inherit-context cenv :return? false)
      (cond-> label (assoc :label label))))

(defmethod parse-expr* 'throw [cenv [_ ex]]
  (-> {:op :throw
       :exception (parse-expr (with-context cenv :expression) ex)}
      (inherit-context cenv :return? false)))

(defn parse-array-creation [cenv type type' args]
  (if (vector? (first args))
    (let [elems (first args)
          arr (gensym)]
      (parse-expr cenv `(let* [~arr (new ~type ~(count elems))]
                          ~@(for [[i init] (map-indexed vector elems)
                                  :let [init' (if (vector? init)
                                                `(new ~(first type) ~init)
                                                init)]]
                              `(~'aset ~arr ~i ~init'))
                          ~arr)))
    (let [cenv' (with-context cenv :expression)]
      (-> {:op :new-array
           :type type'
           :lengths (map (fn [arg]
                           (let [arg' (parse-expr cenv' arg)
                                 cs (t/unary-numeric-promotion (:type arg'))]
                             (apply-conversions cs arg')))
                         args)}
          (inherit-context cenv)))))

(defmethod parse-expr* 'new [cenv [_ type & args]]
  (let [type' (t/tag->type cenv type)]
    (if (t/array-type? type')
      (parse-array-creation cenv type type' args)
      (let [cenv' (with-context cenv :expression)
            args' (map (partial parse-expr cenv') args)
            ctor (t/find-ctor cenv type' (map :type args'))]
        (-> {:op :new
             :type type'
             :access (:access ctor)
             :param-types (:param-types ctor)
             :args args'}
            (inherit-context cenv))))))

(defn parse-field-access [cenv target target-type fname]
  (if (and (t/array-type? target-type) (= fname "length"))
    (-> {:op :array-length
         :type t/INT
         :array target}
        (inherit-context cenv))
    (if-let [{:keys [type used?]} (get (:enclosing-env cenv) fname)]
      (do (reset! used? true)
          (-> {:op :field-access
               :type type
               :class (:class-type cenv)
               :target target
               :name fname}
              (inherit-context cenv)))
      (let [field (find-field cenv target-type fname)]
        (-> {:op :field-access
             :type (:type field)
             :class (:class field)
             :name fname}
            (inherit-context cenv)
            (cond-> target (assoc :target target)))))))

(defn parse-method-invocation [cenv target target-type mname args]
  (let [args' (map (partial parse-expr (with-context cenv :expression)) args)
        methods (t/find-methods cenv target-type mname (map :type args'))
        method (first methods)]
    (-> {:op :method-invocation
         :interface? (:interface? method false)
         :type (:return-type method)
         :access (:access method)
         :param-types (:param-types method)
         :class (:class method)
         :name mname
         :args (mapv apply-conversions (:conversions method) args')}
        (inherit-context cenv)
        (cond-> target (assoc :target target)))))

(defmethod parse-expr* '. [cenv [_ target property & maybe-args :as expr]]
  (if (and (seq? property) (nil? maybe-args))
    (parse-expr cenv `(. ~target ~@property))
    (let [cenv' (with-context cenv :expression)
          target' (when-not (and (symbol? target)
                                 (not (namespace target))
                                 (not (find-lname cenv target))
                                 (t/tag->type cenv target))
                    (parse-expr cenv' target))
          target-type (or (:type target') (t/tag->type cenv target))
          pname (name property)]
      (if (str/starts-with? pname "-")
        (parse-field-access cenv target' target-type (subs pname 1))
        (parse-method-invocation cenv target' target-type pname maybe-args)))))

(defn fold-aget [[_ arr index & indices :as expr]]
  (if (empty? indices)
    expr
    (recur (with-meta `(~'aget (~'aget ~arr ~index) ~@indices) (meta expr)))))

(defmethod parse-expr* 'alength [cenv [_ arr :as expr]]
  (parse-expr cenv (with-meta `(.-length ~arr) (meta expr))))

(defmethod parse-expr* 'aget [cenv [_ arr index & indices :as expr]]
  (if indices
    (parse-expr cenv (fold-aget expr))
    (let [cenv' (with-context cenv :expression)
          arr (parse-expr cenv' arr)
          index' (as-> (parse-expr cenv' index) index'
                   (apply-conversions (t/unary-numeric-promotion (:type index')) index'))]
      (-> {:op :array-access
           :type (t/element-type (:type arr))
           :array arr
           :index index'}
          (inherit-context cenv)))))

(defmethod parse-expr* 'aset [cenv [_ arr index & more :as expr]]
  (if (next more)
    (let [indices (cons index (butlast more))
          form (with-meta
                 `(~'aset (~'aget ~arr ~@(butlast indices)) ~(last indices) ~(last more))
                 (meta expr))]
      (parse-expr cenv form))
    (let [cenv' (with-context cenv :expression)
          arr (parse-expr cenv' arr)
          elem-type (t/element-type (:type arr))
          index' (as-> (parse-expr cenv' index) index'
                   (apply-conversions (t/unary-numeric-promotion (:type index')) index'))
          expr' (parse-expr cenv' (first more))
          cs (t/assignment-conversion cenv' (:type expr') elem-type)]
      (-> {:op :array-update
           :type elem-type
           :array arr
           :index index'
           :expr (apply-conversions cs expr')}
          (inherit-context cenv)))))
