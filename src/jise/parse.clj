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
  {:type (t/tag->type proto-cenv tag :default default-type)
   :access (access-flags modifiers)})

(defn parse-field [proto-cenv [_ fname value :as field]]
  (let [modifiers (modifiers-of field)
        {:keys [access type]} (parse-modifiers proto-cenv modifiers)]
    (cond-> {:name (str fname)
             :type type
             :access access}
      (not (nil? value)) (assoc :value value))))

(defn apply-conversion [{:keys [type] :as x} t]
  (cond->> x
    (not= t type)
    (array-map :op :conversion :type t :src)))

(defn macroexpand [form]
  (if-let [[op & args] (and (seq? form) (symbol? (first form)) (namespace (first form)) form)]
    (with-meta `(. ~(symbol (namespace op)) ~(symbol (name op)) ~@args) (meta form))
    (let [expanded (macroexpand-1 form)]
      (if (identical? expanded form)
        form
        (recur (cond-> expanded
                 (instance? clojure.lang.IObj expanded)
                 (vary-meta merge (meta form))))))))

(declare parse-expr)

(defmulti parse-expr* (fn [cenv expr] (symbol-without-ns (first expr))))
(defmethod parse-expr* :default [cenv expr]
  (let [expanded (macroexpand expr)]
    (if-not (identical? expanded expr)
      (parse-expr cenv expanded)
      (assert false "not supported yet"))))

(defn with-context [x context]
  (assoc x :context context))

(defn inherit-context [x {:keys [context]}]
  (with-context x context))

(defn find-lname [cenv sym]
  ((:lenv cenv) (name sym)))

(defn parse-symbol [cenv sym]
  (if-let [cname (namespace sym)]
    (parse-expr cenv (with-meta `(. ~(symbol cname) ~(symbol (str \- (name sym)))) (meta sym)))
    (if-let [{:keys [index type]} (find-lname cenv sym)]
      (inherit-context {:op :local :index index :type type} cenv)
      (throw (ex-info (str "unknown variable found: " sym) {:variable sym})))))

(defn parse-seq [cenv expr]
  (let [expr' (parse-expr* cenv expr)]
    (if-let [label (:label (meta expr))]
      {:op :labeled :label label :target expr'}
      expr')))

(defn parse-literal [cenv v]
  (if (nil? v)
    (inherit-context {:op :null} cenv)
    (if-let [t (t/object-type v)]
      (merge (inherit-context {:op :literal} cenv)
             (condp #(%1 %2) t
               #{t/BYTE t/SHORT t/INT t/LONG}
               {:type t/INT :value (int v)}

               #{t/FLOAT t/DOUBLE}
               {:type t/DOUBLE :value (double v)}

               {:type t :value v})))))

(defn parse-expr [cenv expr]
  (cond (symbol? expr) (parse-symbol cenv expr)
        (seq? expr) (parse-seq cenv expr)
        :else (parse-literal cenv expr)))

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
        lname' (parse-name cenv lname :default-type (:type init'))]
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
             :body (parse-exprs (with-context cenv' context) body)}
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

(defn init-cenv [proto-cenv cname fields ctors methods]
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
                         {} methods)]
    (assoc-in proto-cenv [:classes cname] {:fields fields' :ctors ctors' :methods methods'})))

(defn parse-class [[_ cname & body :as class]]
  (let [alias (class-alias cname)
        proto-cenv {:class-name cname :classes {}
                    :aliases (cond-> {} (not= cname alias) (assoc alias cname))}
        {:keys [parent interfaces body]} (parse-supers proto-cenv body)
        {:keys [ctors fields methods]} (parse-class-body cname body)
        cenv (init-cenv proto-cenv cname fields ctors methods)]
    {:name (str/replace (str cname) \. \/)
     :access (access-flags (modifiers-of class))
     :parent (or parent t/OBJECT)
     :interfaces interfaces
     :ctors (mapv (partial parse-method cenv true) ctors)
     :fields (mapv (partial parse-field cenv) fields)
     :methods (mapv (partial parse-method cenv false) methods)}))

(defn parse-binary-op [cenv [_ x y] op]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' x)
        rhs (parse-expr cenv' y)
        t (t/wider-type (:type lhs) (:type rhs))]
    {:op op
     :context (:context cenv)
     :lhs (apply-conversion lhs t)
     :rhs (apply-conversion rhs t)}))

(defn parse-arithmetic [cenv expr op]
  (let [{:keys [lhs] :as ret} (parse-binary-op cenv expr op)]
    (assoc ret :type (:type lhs))))

(defmethod parse-expr* '+ [cenv expr]
  (parse-arithmetic cenv expr :add))

(defmethod parse-expr* '- [cenv expr]
  (parse-arithmetic cenv expr :sub))

(defmethod parse-expr* '* [cenv expr]
  (parse-arithmetic cenv expr :mul))

(defmethod parse-expr* '/ [cenv expr]
  (parse-arithmetic cenv expr :div))

(defmethod parse-expr* '% [cenv expr]
  (parse-arithmetic cenv expr :rem))

(defn parse-comparison [cenv expr op]
  (if (:within-conditional? cenv)
    (assoc (parse-binary-op cenv expr op) :type 'boolean)
    (parse-expr (dissoc cenv :within-conditional?) `(if ~expr true false))))

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

(defmethod parse-expr* 'let [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (parse-bindings cenv bindings)
        body' (parse-exprs cenv' body)]
    {:op :let :type (:type body')
     :bindings bindings'
     :body body'}))

(defmethod parse-expr* 'set! [{:keys [context] :as cenv} [_ target expr]]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' target)
        rhs (parse-expr cenv' expr)]
    (if (= (:op lhs) :field-access)
      (cond-> {:op :field-update
               :context context
               :type (:type lhs)
               :class (:class lhs)
               :name (:name lhs)
               :rhs rhs}
        (:target lhs) (assoc :target (:target lhs)))
      {:op :assignment
       :context context
       :type (:type lhs)
       :lhs lhs
       :rhs rhs})))

(defmethod parse-expr* 'inc! [cenv [_ target by]]
  (let [by (or by 1)
        target' (parse-expr (with-context cenv :expression) target)]
    (if (and (= (:op target') :local)
             (= (t/wider-type (:type target') 'int) 'int)
             (pos-int? by))
      (-> {:op :increment, :target target', :type (:type target'), :by by}
          (inherit-context cenv))
      (parse-expr cenv `(set! ~target (~'+ ~target ~by))))))

(defmethod parse-expr* 'dec! [cenv [_ target by]]
  (let [by (or by 1)
        target' (parse-expr (with-context cenv :expression) target)]
    (if (and (= (:op target') :local)
             (= (t/wider-type (:type target') 'int) 'int)
             (pos-int? by))
      (-> {:op :increment, :target target', :type (:type target'), :by (- by)}
          (inherit-context cenv))
      (parse-expr cenv `(set! ~target (~'- ~target ~by))))))

(defmethod parse-expr* 'if [{:keys [context] :as cenv} [_ test then else]]
  (let [cenv' (cond-> cenv
                (not= context :statement)
                (assoc :context :expression))
        test' (parse-expr (-> cenv
                              (with-context :expression)
                              (assoc :within-conditional? true))
                          test)
        then' (parse-expr cenv' then)
        else' (some->> else (parse-expr cenv'))]
    (cond-> {:op :if, :context context, :test test', :then then'}
      else' (assoc :else else')
      (not= context :statement) (assoc :type (:type then')))))

(defn extract-label [expr]
  (:label (meta expr)))

(defmethod parse-expr* 'while [cenv [_ cond & body :as expr]]
  (let [label (extract-label expr)]
    (cond-> {:op :while
             :context (:context cenv)
             :cond (parse-expr (-> cenv
                                   (with-context :expression)
                                   (assoc :within-conditional? true))
                               cond)
             :body (parse-exprs (with-context cenv :statement) body)}
      label (assoc :label label))))

(defmethod parse-expr* 'for [cenv [_ [lname init cond step] & body :as expr]]
  (let [[cenv' bindings'] (parse-bindings cenv [lname init])
        label (extract-label expr)]
    {:op :let
     :bindings bindings'
     :body
     (cond-> {:op :for
              :context (:context cenv)
              :cond (parse-expr (-> cenv'
                                    (with-context :expression)
                                    (assoc :within-conditional? true))
                                cond)
              :step (parse-expr (with-context cenv' :statement) step)
              :body (parse-exprs (with-context cenv' :statement) body)}
       label (assoc :label label))}))

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
         :context (:context cenv)
         :type type'
         :length (parse-expr (with-context cenv :expression) (first args))})
      (let [cenv' (with-context cenv :expression)
            args' (map (partial parse-expr cenv') args)
            ctor (t/find-ctor cenv type' (map :type args'))]
        {:op :new
         :context (:context cenv)
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
        (let [pname' (subs pname 1)
              field (t/find-field cenv target-type pname')]
          (cond-> {:op :field-access
                   :context (:context cenv)
                   :type (:type field)
                   :class (:class field)
                   :name pname'}
            target' (assoc :target target')))
        (let [args' (map (partial parse-expr cenv') maybe-args)
              method (t/find-method cenv target-type pname (map :type args'))]
          (cond-> {:op :method-invocation
                   :context (:context cenv)
                   :type (:return-type method)
                   :access (:access method)
                   :arg-types (:arg-types method)
                   :class (:class method)
                   :name pname
                   :args args'}
            target' (assoc :target target')))))))

(defmethod parse-expr* 'aget [cenv [_ arr index]]
  (let [cenv' (with-context cenv :expression)
        arr (parse-expr cenv' arr)]
    {:op :array-access
     :context (:context cenv)
     :type (t/element-type (:type arr))
     :array arr
     :index (parse-expr cenv' index)}))

(defmethod parse-expr* 'aset [cenv [_ arr index expr]]
  (let [cenv' (with-context cenv :expression)
        arr (parse-expr cenv' arr)]
    {:op :array-update
     :context (:context cenv)
     :type (t/element-type (:type arr))
     :array arr
     :index (parse-expr cenv' index)
     :expr (parse-expr cenv' expr)}))
