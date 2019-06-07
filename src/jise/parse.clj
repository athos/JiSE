(ns jise.parse
  (:refer-clojure :exclude [find-var])
  (:require [clojure.string :as str]
            [jise.misc :as misc]
            [jise.simplify :as simp]
            [jise.type :as t])
  (:import [clojure.asm Type]
           [clojure.java.api Clojure]))

(def ^:dynamic *line* nil)
(def ^:dynamic *column* nil)

(defmacro with-line&column-of [x & body]
  `(let [{line# :line column# :column} (meta ~x)]
     (if (and line# column#)
       (binding [*line* line# *column* column#]
         ~@body)
       (do ~@body))))

(defn stringify-type [t]
  (if (nil? t)
    "<null>"
    (str (t/type->tag t))))

(defmacro error [msg & [data]]
  `(let [msg# (str "Error: " ~msg " (" *file* \: *line* \: *column* ")")
         data# (merge {:line *line* :column *column*} ~data)]
     (throw (ex-info msg# data#))))

(defmacro error-on-incompatible-types [expected actual]
  `(error (format "incompatible types: %s cannot be converted to %s"
                  (stringify-type ~actual)
                  (stringify-type ~expected))))

(defn modifiers-of [[_ name :as form]]
  (merge (meta form) (meta name)))

(defn access-flags [modifiers]
  (let [access (-> modifiers
                   (select-keys [:abstract :static :public :protected :private :final :transient :volatile])
                   keys
                   set)]
    (cond-> access
      (every? (complement #{:public :protected :private}) access)
      (conj :package))))

(defn resolve-type [proto-cenv tag & {:keys [allow-vararg-param-type?]}]
  (try
    (t/tag->type proto-cenv tag
                 :allow-vararg-param-type? allow-vararg-param-type?)
    (catch Exception e
      (case (:cause (ex-data e))
        (:unresolved-type :invalid-vararg-param-type)
        (error (ex-message e) (dissoc (ex-data e) :cause))

        (error (str "cannot resolve type " (pr-str tag)) {:unresolved-type tag})))))

(defn parse-modifiers
  [proto-cenv {:keys [tag] :as modifiers} & {:keys [default-type allow-vararg-param?]}]
  {:type (if (nil? tag)
           (or default-type t/OBJECT)
           (resolve-type proto-cenv tag :allow-vararg-param-type? allow-vararg-param?))
   :access (access-flags modifiers)})

(defn field-init-value
  ([field]
   (let [access (access-flags (modifiers-of field))]
     (field-init-value access field)))
  ([access [_ _ value :as field]]
   (and (:static access) value (simp/simplify {} value))))

(defn parse-field [proto-cenv [_ fname value :as field]]
  (let [modifiers (modifiers-of field)
        {:keys [access type]} (parse-modifiers proto-cenv modifiers)
        value' (field-init-value access field)]
    (cond-> {:name (str fname)
             :type type
             :access access}
      value'
      (assoc :value value'))))

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

(defn first-meaningful-node [node]
  (if (= (:op node) :do)
    (recur (first (:exprs node)))
    node))

(defn apply-conversions [conversions src]
  (reduce (fn [src {:keys [conversion to]}]
            (-> {:op conversion
                 :type to
                 :src (with-context src :expression)}
                (inherit-context src)))
          src
          conversions))

(defn ensure-type [cenv type src & {:keys [context] :or {context :assignment}}]
  (let [conv (case context
               :assignment t/assignment-conversion
               :casting t/casting-conversion)]
    (if-let [cs (conv cenv (:type src) type)]
      (-> (apply-conversions cs src)
          (inherit-context cenv))
      (error-on-incompatible-types type (:type src)))))

(defn find-in-current-class [cenv & ks]
  (get-in cenv (into [:classes (:class-name cenv)] ks)))

(defn find-lname [cenv sym]
  (get (:lenv cenv) (name sym)))

(defn find-var [cenv sym]
  (when-let [var (resolve sym)]
    (when-not (:macro (meta var))
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
            (get-in [:var->entry vname]))))))

(def VAR_TYPE (Type/getType clojure.lang.Var))

(defn find-field [cenv class fname]
  (or (t/find-field cenv (:class-type cenv) class fname)
      (when (and (= class (:class-type cenv))
                 (get-in @(:vars cenv) [:fields fname]))
        {:class class :type VAR_TYPE :access #{:public :static}})))

(declare parse-expr parse-method-invocation)

(defn parse-sugar [{:keys [class-type] :as cenv} [op :as expr]]
  (or (when-let [maybe-array (or (find-lname cenv op)
                                 (find-field cenv class-type (str op)))]
        (if (t/array-type? (:type maybe-array))
          (parse-expr cenv (with-meta `(~'aget ~@expr) (meta expr)))
          (error (format "array required, but %s found" (stringify-type (:type maybe-array))))))
      (when (t/get-methods cenv class-type class-type (str op) (count (rest expr)))
        (parse-method-invocation cenv nil class-type (str op) (rest expr)))
      (when-let [{:keys [var field-name]} (and (namespace op) (find-var cenv op))]
        (when-not (:macro (meta var))
          (let [form `(.invoke (. ~(:class-name cenv) ~(symbol (str \- field-name)))
                               ~@(rest expr))]
            (parse-expr cenv (with-meta form (meta expr))))))))

(defmulti parse-expr* (fn [cenv expr] (misc/fixup-ns (first expr))))
(defmethod parse-expr* :default [cenv expr]
  (let [expanded (misc/macroexpand cenv expr)]
    (if-not (identical? expanded expr)
      (parse-expr cenv expanded)
      (or (parse-sugar cenv expr)
          (error (str "unsupported expression: " (pr-str expr)) {:expr expr})))))

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
        (if-let [{:keys [type foreign?] :as local} (find-lname cenv sym)]
          (if foreign?
            (parse-as-field cenv 'this)
            (inherit-context {:op :local :type type :local local} cenv))
          (if-let [f (find-field cenv (:class-type cenv) (name sym))]
            (let [target (if (:static (:access f)) (:class-name cenv) 'this)]
              (parse-as-field cenv target))
            (error (str "cannot find symbol: " sym) {:variable sym})))))))

(defn parse-seq [cenv expr]
  (if-let [tag (:tag (meta expr))]
    (parse-expr cenv `(~'cast ~tag ~(vary-meta expr dissoc :tag)))
    (let [{:keys [tag line label]} (meta expr)
          cenv' (if label (inherit-context cenv cenv :return? false) cenv)
          expr' (if (symbol? (first expr))
                  (binding [*line* (or (:line (meta expr)) *line*)
                            *column* (or (:column (meta expr)) *column*)]
                    (parse-expr* cenv' expr))
                  (error (str "unsupported expression: " (pr-str expr)) {:expr expr}))]
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

               {:type t :value v}))
      (error (str (pr-str v) " cannot be used as literal")))))

(defn parse-expr [{:keys [return-type] :as cenv} expr]
  (let [expr' (cond (symbol? expr) (parse-symbol cenv expr)
                    (seq? expr) (parse-seq cenv expr)
                    :else (parse-literal cenv expr))]
    (if (and (:return (:context expr'))
             (not= return-type t/VOID)
             (not= (or (:type expr') t/VOID) return-type))
      (let [conv #(ensure-type cenv return-type %)]
        (if (= (:type expr') t/VOID)
          ;; insert implicit (do ... nil)
          (-> {:op :do
               :type return-type
               :exprs [(with-context expr' :statement)
                       (conv (parse-literal cenv nil))]}
              (inherit-context cenv :return? false))
          (conv expr')))
      expr')))

(defn  parse-exprs [cenv body]
  (let [cenv' (with-context cenv :statement)
        exprs' (mapv parse-expr (repeat cenv') (butlast body))
        last' (parse-expr cenv (last body))]
    (-> {:op :do :type (:type last')
         :exprs (conj exprs' last')}
        (inherit-context cenv :return? false))))

(defn parse-name [proto-cenv name & {:keys [default-type allow-vararg-param?]}]
  (let [{:keys [access type]} (parse-modifiers proto-cenv (meta name)
                                               :default-type default-type
                                               :allow-vararg-param? allow-vararg-param?)]
    {:name name
     :type type
     :access access}))

(defn next-index [cenv type]
  (first (swap-vals! (:next-index cenv) + (t/type-category type))))

(defn parse-binding [cenv lname init allow-vararg-param?]
  (let [init' (some->> init (parse-expr (with-context cenv :expression)))
        lname' (parse-name cenv lname
                           :default-type (:type init')
                           :allow-vararg-param? allow-vararg-param?)
        init' (when init' (ensure-type cenv (:type lname') init' :context :casting))]
    (-> lname'
        (update :name name)
        (assoc :index (next-index cenv (:type lname')))
        (cond-> init' (assoc :init init')))))

(defn parse-bindings [cenv bindings & {:keys [method-params?]}]
  (loop [[lname init & bindings] bindings
         cenv' (with-context cenv :expression)
         allow-vararg-param? false
         ret []]
    (if lname
      (if (= lname '&)
        (if method-params?
          (recur bindings cenv' true ret)
          (error "varargs parameter is only allowed in method signature"))
        (if (and allow-vararg-param? (seq bindings))
          (error "varargs parameter must be the last parameter")
          (let [b (parse-binding cenv' lname init (and allow-vararg-param? (empty? bindings)))
                cenv' (assoc-in cenv' [:lenv (:name b)]
                                (cond-> b method-params? (assoc :param? true)))]
            (when (and allow-vararg-param? (not (t/array-type? (:type b))))
              (error "varargs parameter must be of array type"))
            (recur bindings cenv' allow-vararg-param? (conj ret b)))))
      [(inherit-context cenv' cenv) ret])))

(declare parse-ctor-invocation)

(defn inject-ctor-invocation [cenv body]
  (or (when-let [node (first-meaningful-node body)]
        (when-not (= (:op node) :ctor-invocation)
          (let [non-empty? (not (and (= (:op node) :null) (:return (:context node))))
                cenv' (cond-> cenv non-empty? (with-context :statement))]
            (-> {:op :do :type (:type body)
                 :exprs (cond-> [(parse-ctor-invocation cenv' '(super))]
                          ;; omit entire body when body is empty `do`
                          non-empty? (conj body))}
                (inherit-context cenv :return? false)))))
      body))

(defn parse-method [cenv ctor? [_ mname args & body :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access type]} (parse-modifiers cenv modifiers :default-type t/VOID)
        init-lenv (if (:static access)
                    {}
                    {"this" {:index 0 :type (:class-type cenv)
                             :access #{} :param? true}})
        init-index (count init-lenv)
        [cenv' args'] (parse-bindings (assoc cenv
                                             :lenv (merge init-lenv (:enclosing-env cenv))
                                             :next-index (atom init-index))
                                      (interleave args (repeat nil))
                                      :method-params? true)
        return-type (if ctor? t/VOID type)
        context (if (= return-type t/VOID) :statement :expression)
        body' (when-not (:abstract access)
                (with-line&column-of method
                  (let [cenv' (assoc cenv'
                                     :return-type return-type
                                     :context #{context :tail :return})]
                    (cond->> (parse-exprs cenv' body)
                      ctor? (inject-ctor-invocation cenv')))))]
    (cond-> {:return-type return-type
             :args args'
             :access access
             :body body'}
      ctor? (assoc :ctor? ctor?)
      (some #{'&} args) (update :access conj :varargs)
      (not ctor?) (assoc :name (str mname)))))

(defn parse-supers [proto-cenv [maybe-supers & body]]
  (let [supers (when (vector? maybe-supers) maybe-supers)
        supers' (map (partial resolve-type proto-cenv) supers)
        {[parent] false
         interfaces true} (group-by #(.isInterface (t/type->class %)) supers')]
    {:parent parent
     :interfaces interfaces
     :body (cond->> body (nil? supers) (cons maybe-supers))}))

(defn class-alias [cname]
  (symbol (str/replace cname #"^.*\.([^.]+)" "$1")))

(defn convert-def-to-set [alias [_ name init :as def]]
  (when (and init (not (field-init-value def)))
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

(defn parse-method-signature [proto-cenv [_ name args :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [type access]} (parse-modifiers proto-cenv modifiers :default-type t/VOID)
        varargs? (volatile! false)
        param-types (reduce (fn [pts [prev arg]]
                              (if (= arg '&)
                                pts
                                (let [vararg-param? (= prev '&)]
                                  (when vararg-param? (vreset! varargs? true))
                                  (->> (parse-name proto-cenv arg :allow-vararg-param? vararg-param?)
                                       :type
                                       (conj pts)))))
                            []
                            (partition 2 1 (cons nil args)))]
    {:access (cond-> access @varargs? (conj :varargs))
     :return-type type
     :param-types param-types}))

(defn init-cenv [proto-cenv cname parent interfaces fields ctors methods initializer]
  (let [fields' (into {} (map (fn [[_ name :as field]]
                                (with-line&column-of field
                                  [(str name) (parse-field proto-cenv field)])))
                      fields)
        ctors' (reduce (fn [cs [_ _ args :as ctor]]
                         (with-line&column-of ctor
                           (let [access (access-flags (modifiers-of ctor))
                                 param-types (mapv #(:type (parse-name proto-cenv %)) args)]
                             (conj cs {:access access :param-types param-types}))))
                       [] ctors)
        methods' (reduce (fn [m [_ name :as method]]
                           (with-line&column-of method
                             (let [method' (parse-method-signature proto-cenv method)]
                               (update m (str name) (fnil conj []) method'))))
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
                              (~'cast clojure.lang.Var (Clojure/var ~(str var-name))))))
            fields
            (:var->entry @vars))))

(defn parse-class
  ([class] (parse-class {} class))
  ([enclosing-env [_ cname & body :as class]]
   (let [alias (class-alias cname)
         proto-cenv {:class-name cname :classes {cname {}}
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
                  (as-> cenv (assoc cenv :class-type (resolve-type cenv cname))))
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
                            (with-line&column-of class
                              (let [m `^:static (~'defm ~'<clinit> [] ~@init)]
                                (-> (parse-method cenv false m)
                                    (assoc :static-initializer? true)))))
      :ctors ctors'
      :methods methods'
      :fields (mapv (partial parse-field cenv)
                    (concat fields synthesized-fields))})))

(defmacro error-on-bad-operand-type [op-name t]
  `(error (format "bad operand type %s for unary operator '%s'"
                  (stringify-type ~t) ~op-name)))

(defn ensure-numeric [cenv x op-name]
  (if-let [cs (t/unary-numeric-promotion (:type x))]
    (-> (apply-conversions cs x)
        (inherit-context cenv))
    (error-on-bad-operand-type op-name (:type x))))

(defn parse-unary-op [cenv [op-name x] op]
  (let [cenv' (with-context cenv :expression)
        x' (ensure-numeric cenv' (parse-expr cenv' x) op-name)]
    (-> {:op op
         :type (:type x')
         :operand x'}
        (inherit-context cenv))))

(defmacro error-on-bad-operand-types [op-name t1 t2]
  `(error (str "bad operand types for binary operator '" ~op-name "'\n"
               "  first type: " (stringify-type ~t1) "\n"
               "  second type: " (stringify-type ~t2))))

(defn parse-binary-op
  ([cenv [op-name x y] op]
   (let [cenv' (with-context cenv :expression)
         lhs (parse-expr cenv' x)
         rhs (parse-expr cenv' y)]
     (parse-binary-op cenv lhs rhs op op-name)))
  ([cenv lhs rhs op op-name]
   (if-let [[cl cr] (t/binary-numeric-promotion (:type lhs) (:type rhs))]
     (-> {:op op
          :lhs (apply-conversions cl lhs)
          :rhs (apply-conversions cr rhs)}
         (inherit-context cenv))
     (error-on-bad-operand-types op-name (:type lhs) (:type rhs)))))

(defn parse-arithmetic [cenv expr op]
  (let [{:keys [lhs] :as ret} (parse-binary-op cenv expr op)]
    (assoc ret :type (:type lhs))))

(defn coerce-to-primitive [cenv [op x]]
  (let [cenv' (with-context cenv :expression)]
    (ensure-numeric cenv (parse-expr cenv' x) op)))

(defn fold-binary-op [[op x y & more :as expr]]
  (if more
    (recur (with-meta `(~op (~op ~x ~y) ~@more) (meta expr)))
    expr))

(defn error-on-missing-arguments [op-name num varargs?]
  (error (str (when varargs?
                "at least ")
              num (if (= num 1) " argument " " arguments ")
              "required for operator '" op-name "'")))

(defn ensure-sufficient-arguments [num [op-name & args] & {:keys [varargs?]}]
  (let [nargs (count args)]
    (when-not (if varargs?
                (>= nargs num)
                (= nargs num))
      (error-on-missing-arguments op-name num varargs?))))

(defmethod parse-expr* '+ [cenv [_ & args :as expr]]
  (case (count args)
    0 (parse-literal cenv 0)
    1 (coerce-to-primitive cenv expr)
    2 (parse-arithmetic cenv expr :add)
    (parse-expr cenv (fold-binary-op expr))))

(defmethod parse-expr* '- [cenv [_ & args :as expr]]
  (ensure-sufficient-arguments 1 expr :varargs? true)
  (case (count args)
    1 (parse-unary-op cenv expr :neg)
    2 (parse-arithmetic cenv expr :sub)
    (parse-expr cenv (fold-binary-op expr))))

(defmethod parse-expr* '* [cenv [_ & args :as expr]]
  (case (count args)
    0 (parse-literal cenv 1)
    1 (coerce-to-primitive cenv expr)
    2 (parse-arithmetic cenv expr :mul)
    (parse-expr cenv (fold-binary-op expr))))

(defmethod parse-expr* '/ [cenv [_ & args :as expr]]
  (ensure-sufficient-arguments 1 expr :varargs? true)
  (case (count args)
    1 (parse-expr cenv (with-meta `(~'/ 1 ~(first args)) (meta expr)))
    2 (parse-arithmetic cenv expr :div)
    (parse-expr cenv (fold-binary-op expr))))

(defmethod parse-expr* '% [cenv expr]
  (ensure-sufficient-arguments 2 expr)
  (parse-arithmetic cenv expr :rem))

(defmethod parse-expr* '& [cenv expr]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (parse-arithmetic cenv (fold-binary-op expr) :bitwise-and))

(defmethod parse-expr* '| [cenv expr]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (parse-arithmetic cenv (fold-binary-op expr) :bitwise-or))

(defmethod parse-expr* 'xor [cenv expr]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (parse-arithmetic cenv (fold-binary-op expr) :bitwise-xor))

(defmethod parse-expr* '! [cenv [_ operand :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-expr cenv (with-meta `(~'xor ~operand -1) (meta expr))))

(defn parse-shift [cenv [op-name x y :as expr] op]
  (ensure-sufficient-arguments 2 expr)
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' x)
        rhs (parse-expr cenv' y)
        lhs' (when-let [cs (t/unary-numeric-promotion (:type lhs))]
               (apply-conversions cs lhs))
        rhs' (when-let [cs (t/unary-numeric-promotion (:type rhs))]
               (apply-conversions cs rhs))]
    (when-not (and lhs' (t/integral-type? (:type lhs'))
                   rhs' (t/integral-type? (:type rhs')))
      (error-on-bad-operand-types op-name (:type lhs) (:type rhs)))
    (-> {:op op
         :type (:type lhs')
         :lhs lhs'
         :rhs (let [cs (t/casting-conversion cenv' (:type rhs') t/INT)]
                (apply-conversions cs rhs'))}
        (inherit-context cenv))))

(defmethod parse-expr* '<< [cenv expr]
  (parse-shift cenv expr :shift-left))

(defmethod parse-expr* '>> [cenv expr]
  (parse-shift cenv expr :shift-right))

(defmethod parse-expr* '>>> [cenv expr]
  (parse-shift cenv expr :logical-shift-right))

(defn parse-equal [cenv lhs rhs op op-name]
  (if (and (not (t/numeric-type? (:type lhs)))
           (not (t/numeric-type? (:type rhs))))
    (if-let [[lhs' rhs'] (or (when-let [cs (t/casting-conversion cenv (:type lhs) (:type rhs))]
                               [(apply-conversions cs lhs) rhs])
                             (when-let [cs (t/casting-conversion cenv (:type rhs) (:type lhs))]
                               [lhs (apply-conversions cs rhs)]))]
      {:op op
       :type t/BOOLEAN
       :lhs lhs
       :rhs rhs}
      (error-on-bad-operand-types op-name (:type lhs) (:type rhs)))
    (-> (parse-binary-op cenv lhs rhs op op-name)
        (assoc :type t/BOOLEAN))))

(defn fold-comparison [[op & args :as expr]]
  (with-meta
    `(~'and ~@(map (fn [[x y]] `(~op ~x ~y)) (partition 2 1 args)))
    (meta expr)))

(defn parse-comparison
  ([cenv [op-name x y & more :as expr] op]
   (if (:conditional (:context cenv))
     (if more
       (parse-expr cenv (fold-comparison expr))
       (let [cenv' (with-context cenv :expression)
             x' (parse-expr cenv' x)
             y' (parse-expr cenv' y)]
         (parse-comparison cenv' x' y' op op-name)))
     (parse-expr cenv `(if ~expr true false))))
  ([cenv lhs rhs op op-name]
   (if ('#{== !=} op-name)
     (parse-equal cenv lhs rhs op op-name)
     (-> (parse-binary-op cenv lhs rhs op op-name)
         (assoc :type t/BOOLEAN)))))

(defn parse-cmp-0 [cenv x y op default-op default-op-name default]
  (let [x=0? (= x 0)
        z (if x=0? y x)]
    (if (= z 0)
      (parse-expr cenv default)
      (let [cenv' (with-context cenv :expression)
            z' (parse-expr cenv' z)]
        (if (= (:type z') t/INT)
          {:op op
           :type t/BOOLEAN
           :operand z'}
          (let [zero (parse-literal cenv' 0)
                [x' y'] (if x=0? [zero z'] [z' zero])]
            (parse-comparison cenv' x' y' default-op default-op-name)))))))

(defn parse-eq-null [cenv x y op op-name default]
  (let [x-nil? (nil? x)
        z (if x-nil? y x)]
    (if (nil? z)
      (parse-literal cenv default)
      (let [{:keys [type] :as z'} (parse-expr (with-context cenv :expression) z)]
        (if-not (t/primitive-type? type)
          {:op op
           :type t/BOOLEAN
           :operand z'}
          (let [[t1 t2] (if x-nil? [nil type] [type nil])]
            (error-on-bad-operand-types op-name t1 t2)))))))

(defmethod parse-expr* '== [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when-not more
        (cond (or (= x 0) (= y 0)) (parse-cmp-0 cenv x y :eq-0 :eq '== true)
              (or (nil? x) (nil? y)) (parse-eq-null cenv x y :eq-null '== true)))
      (parse-comparison cenv expr :eq)))

(defmethod parse-expr* '!= [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when-not more
        (cond (or (= x 0) (= y 0)) (parse-cmp-0 cenv x y :ne-0 :ne '!= false)
              (or (nil? x) (nil? y)) (parse-eq-null cenv x y :ne-null '!= false)))
      (parse-comparison cenv expr :ne)))

(defmethod parse-expr* '< [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv x y :lt-0 :lt '< false)
    (parse-comparison cenv expr :lt)))

(defmethod parse-expr* '> [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv x y :gt-0 :gt '> false)
    (parse-comparison cenv expr :gt)))

(defmethod parse-expr* '<= [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv x y :le-0 :le '<= true)
    (parse-comparison cenv expr :le)))

(defmethod parse-expr* '>= [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if (and (nil? more) (or (= x 0) (= y 0)))
    (parse-cmp-0 cenv x y :ge-0 :ge '>= true)
    (parse-comparison cenv expr :ge)))

(defn unbox-if-needed [x]
  (if-let [cs (t/unboxing-conversion (:type x))]
    (apply-conversions cs x)
    x))

(defmethod parse-expr* 'and [cenv [_ & exprs :as expr]]
  (if (:conditional (:context cenv))
    (case (count exprs)
      0 (parse-literal cenv true)
      1 (parse-expr cenv (first exprs))
      (let [exprs' (mapv #(unbox-if-needed (parse-expr cenv %)) exprs)]
        (when-let [[e1 e2] (some (fn [[e1 e2]]
                                   (when-not (and (= (:type e1) t/BOOLEAN)
                                                  (= (:type e2) t/BOOLEAN))
                                     [e1 e2]))
                                 (partition 2 1 exprs'))]
          (error-on-bad-operand-types 'and (:type e1) (:type e2)))
        {:op :and
         :type t/BOOLEAN
         :exprs exprs'}))
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
      0 (parse-literal cenv false)
      1 (parse-expr cenv (first exprs))
      (let [exprs' (mapv #(unbox-if-needed (parse-expr cenv %)) exprs)]
        (when-let [[e1 e2] (some (fn [[e1 e2]]
                                   (when-not (and (= (:type e1) t/BOOLEAN)
                                                  (= (:type e2) t/BOOLEAN))
                                     [e1 e2]))
                                 (partition 2 1 exprs'))]
          (error-on-bad-operand-types 'and (:type e1) (:type e2)))
        {:op :or
         :type t/BOOLEAN
         :exprs (mapv negate-expr (butlast exprs'))
         :expr (last exprs')}))
    (parse-expr cenv `(if ~expr true false))))

(defmethod parse-expr* 'not [cenv [_ operand :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (if (:conditional (:context cenv))
    (let [{:keys [type] :as operand'} (parse-expr cenv operand)]
      (if (#{t/BOOLEAN t/BOOLEAN_CLASS} type)
        (negate-expr operand')
        (error-on-bad-operand-type 'not type)))
    (parse-expr cenv `(if ~expr true false))))

(defn parse-cast [cenv type x]
  (let [cenv' (with-context cenv :expression)]
    (ensure-type cenv type (parse-expr cenv' x) :context :casting)))

(defmethod parse-expr* 'boolean [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/BOOLEAN x))

(defmethod parse-expr* 'byte [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/BYTE x))

(defmethod parse-expr* 'char [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/CHAR x))

(defmethod parse-expr* 'short [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/SHORT x))

(defmethod parse-expr* 'int [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/INT x))

(defmethod parse-expr* 'long [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/LONG x))

(defmethod parse-expr* 'float [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/FLOAT x))

(defmethod parse-expr* 'double [cenv [_ x :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-cast cenv t/DOUBLE x))

(defmethod parse-expr* 'cast [cenv [_ t x :as expr]]
  (ensure-sufficient-arguments 2 expr)
  (parse-cast cenv (resolve-type cenv t) x))

(defmethod parse-expr* '= [cenv [_ x y :as expr]]
  (ensure-sufficient-arguments 2 expr)
  (parse-expr cenv (with-meta `(.equals ~x ~y) (meta expr))))

(defmethod parse-expr* 'nil? [cenv [_ arg :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-expr cenv (with-meta `(~'== ~arg nil) (meta expr))))

(defmethod parse-expr* `nil? [cenv [_ arg :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-expr cenv (with-meta `(~'== ~arg nil) (meta expr))))

(defmethod parse-expr* 'str [cenv [_ & args :as expr]]
  (if (every? string? args)
    (parse-expr cenv (apply str args))
    (let [form `(-> (new StringBuilder)
                    ~@(map (fn [arg] `(.append ~arg)) args)
                    .toString)]
      (parse-expr cenv (with-meta form (meta expr))))))

(defmethod parse-expr* 'instance? [cenv [_ c x :as expr]]
  (ensure-sufficient-arguments 2 expr)
  (let [c' (resolve-type cenv c)
        x' (parse-expr (with-context cenv :expression) x)]
    (when (t/primitive-type? c')
      (error (str "unexpected type\n"
                  "  required: class or array\n"
                  "  found:    " (t/type->tag c'))))
    (when-not (t/casting-conversion cenv (:type x') c')
      (error-on-incompatible-types c' (:type x')))
    (-> {:op :instance?
         :type t/BOOLEAN
         :class c'
         :operand x'}
        (inherit-context cenv))))

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

(defn parse-field-update [cenv {:keys [field] :as lhs} rhs]
  (cond (and (:final (:access field))
             (some-> (:initialized? field) deref))
        (error (str "cannot assign a value to final variable " (:name field)))

        (:foreign? field)
        (error (str "cannot assign a value to foreign variable " (:name field)))

        :else
        (do (some-> (:initialized? field) (reset! true))
            (-> {:op :field-update
                 :type (:type lhs)
                 :field field
                 :rhs rhs}
                (inherit-context cenv)
                (cond-> (:target lhs) (assoc :target (:target lhs)))))))

(defmethod parse-expr* 'set! [cenv [_ target expr]]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' target)
        rhs (->> (parse-expr cenv' expr)
                 (ensure-type cenv' (:type lhs)))]
    (case (:op lhs)
      :field-access
      (parse-field-update cenv lhs rhs)

      :array-access
      (-> {:op :array-update
           :type (:type lhs)
           :array (:array lhs)
           :index (:index lhs)
           :expr rhs}
          (inherit-context cenv))

      (let [local (:local lhs)]
        (if (:final (:access local))
          (if (:param? local)
            (error (str "final parameter " target " may not be assigned"))
            (error (str "cannot assign a value to final variable " target)))
          (-> {:op :assignment
               :type (:type lhs)
               :lhs lhs
               :rhs rhs}
              (inherit-context cenv)))))))

(defn parse-increment [cenv target by max-value default-op op-name]
  (let [by (or by 1)
        {:keys [type] :as target'} (parse-expr (with-context cenv :expression) target)]
    (when-not (t/numeric-type? type)
      (error-on-bad-operand-type op-name type))
    (if (and (= (:op target') :local)
             (or (= type t/INT)
                 (when-let [{:keys [to]} (t/widening-primitive-conversion type t/INT)]
                   (= to t/INT)))
             (int? by)
             (<= 0 by max-value))
      (let [local (:local target')]
        (if (:final (:access local))
          (if (:param? local)
            (error (str "final parameter " target " may not be assigned"))
            (error (str "cannot assign a value to final variable " target)))
          (-> {:op :increment, :target target', :type type, :by by}
              (inherit-context cenv))))
      (parse-expr cenv `(set! ~target (~default-op ~target ~by))))))

(defmethod parse-expr* 'inc! [cenv [_ target by]]
  (parse-increment cenv target by Byte/MAX_VALUE '+ 'inc!))

(defmethod parse-expr* 'dec! [cenv [_ target by]]
  (parse-increment cenv target by (- Byte/MIN_VALUE) '- 'dec!))

(defmethod parse-expr* 'if [cenv [_ test then else]]
  (cond (true? test) (parse-expr cenv then)
        (false? test) (parse-expr cenv else)
        :else
        (let [test' (as-> (parse-expr (with-context cenv :conditional) test) test'
                      (ensure-type (with-context cenv :expression)
                                   t/BOOLEAN test' :context :casting))
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
        cond' (as-> (parse-expr (with-context cenv :conditional) cond) cond'
                (ensure-type (with-context cenv :expression)
                             t/BOOLEAN cond' :context :casting))]
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
          cond' (as-> (parse-expr (with-context cenv' :conditional) cond) cond'
                  (ensure-type (with-context cenv :expression)
                               t/BOOLEAN cond' :context :casting))
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
  (let [class' (resolve-type cenv class)
        [cenv' [b]] (parse-bindings cenv [(with-meta lname {:tag class}) nil])
        body' (parse-expr cenv' (append-finally cenv finally-clause body))]
    {:type (:type body')
     :local b
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

(defmethod parse-expr* 'throw [cenv [_ ex :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (-> {:op :throw
       :exception (parse-expr (with-context cenv :expression) ex)}
      (inherit-context cenv :return? false)))

(defn ensure-numeric-int [cenv x]
  (or (when-let [x' (try
                      (ensure-numeric cenv x nil)
                      (catch Exception _))]
        (when (= (:type x') t/INT)
          x'))
      (error-on-incompatible-types t/INT (:type x))))

(defn parse-array-creation [cenv type' [_ type & args :as expr]]
  (if (vector? (first args))
    (let [elems (first args)
          arr (gensym)
          form `(let* [~arr (new ~type ~(count elems))]
                  ~@(for [[i init] (map-indexed vector elems)
                          :let [init' (if (vector? init)
                                        `(new ~(first type) ~init)
                                        init)]]
                      `(~'aset ~arr ~i ~init'))
                  ~arr)]
      (parse-expr cenv (with-meta form (meta expr))))
    (let [cenv' (with-context cenv :expression)]
      (-> {:op :new-array
           :type type'
           :lengths (map #(ensure-numeric-int cenv' (parse-expr cenv' %)) args)}
          (inherit-context cenv)))))

(defn args-for [cenv {:keys [param-types] :as ctor-or-method} args args']
  (let [ncs (count (:conversions ctor-or-method))
        varargs (when (not= (count param-types) ncs)
                  (let [vararg-type (peek param-types)
                        varargs-form `(new ~(t/type->tag vararg-type)
                                           ~(vec (drop ncs args)))]
                    (parse-expr cenv varargs-form)))]
    (cond-> (mapv apply-conversions (:conversions ctor-or-method) args')
      varargs
      (conj varargs))))

(defmethod parse-expr* 'new [cenv [_ type & args :as expr]]
  (let [type' (resolve-type cenv type)]
    (if (t/array-type? type')
      (parse-array-creation cenv type' expr)
      (let [cenv' (with-context cenv :expression)
            args' (map (partial parse-expr cenv') args)
            ctors (t/find-ctors cenv (:class-type cenv) type' (map :type args'))]
        (when-let [ctor (first ctors)]
          (-> {:op :new
               :type type'
               :ctor (assoc ctor :class type')
               :args (args-for cenv' ctor args args')}
              (inherit-context cenv)))))))

(defn parse-ctor-invocation [cenv [op & args]]
  (let [super? (= (misc/symbol-without-jise-ns op) 'super)
        cenv' (with-context cenv :expression)
        args' (map (partial parse-expr cenv') args)
        class (if super?
                (find-in-current-class cenv :parent)
                (:class-type cenv))
        ctors (t/find-ctors cenv (:class-type cenv) class (map :type args'))
        initializer (when super? (find-in-current-class cenv :initializer))]
    (when-let [ctor (first ctors)]
      (let [node {:op :ctor-invocation
                  :ctor (assoc ctor :class class)
                  :args (args-for cenv' ctor args args')}]
        (if initializer
          (-> {:op :do
               :exprs [(with-context node :statement)
                       (parse-expr cenv initializer)]}
              (inherit-context cenv :return? false))
          (inherit-context node cenv))))))

(defmethod parse-expr* 'this [cenv expr]
  (parse-ctor-invocation cenv expr))

(defmethod parse-expr* 'super [cenv expr]
  (parse-ctor-invocation cenv expr))

(defn parse-field-access [cenv target target-type fname]
  (if (and (t/array-type? target-type) (= fname "length"))
    (-> {:op :array-length
         :type t/INT
         :array target}
        (inherit-context cenv))
    (if-let [{:keys [type used?] :as field} (get (:enclosing-env cenv) fname)]
      (do (reset! used? true)
          (-> {:op :field-access
               :type type
               :field (assoc field :class (:class-type cenv) :name fname)
               :target target}
              (inherit-context cenv)))
      (let [{:keys [access] :as field} (find-field cenv target-type fname)]
        (if (and (:final access) (contains? field :value))
          (parse-literal cenv (:value field))
          (-> {:op :field-access
               :type (:type field)
               :field (assoc field :name fname)}
              (inherit-context cenv)
              (cond-> target (assoc :target target))))))))

(defn parse-method-invocation [cenv target target-type mname args]
  (let [cenv' (with-context cenv :expression)
        args' (map (partial parse-expr cenv') args)
        methods (t/find-methods cenv (:class-type cenv) target-type mname (map :type args'))]
    (when-let [method (first methods)]
      (let [target' (if (and (nil? target) (not (:static (:access method))))
                      (parse-expr cenv' 'this)
                      target)]
        (-> {:op :method-invocation
             :type (:return-type method)
             :method (assoc method :name mname)
             :args (args-for cenv' method args args')}
            (inherit-context cenv)
            (cond-> target' (assoc :target target')))))))

(defmethod parse-expr* '. [cenv [_ target property & maybe-args :as expr]]
  (if (and (seq? property) (nil? maybe-args))
    (parse-expr cenv `(. ~target ~@property))
    (let [cenv' (with-context cenv :expression)
          target' (when-not (and (symbol? target)
                                 (not (namespace target))
                                 (not (find-lname cenv target))
                                 (t/tag->type cenv target :throws-on-failure? false))
                    (parse-expr cenv' target))
          target-type (or (:type target') (resolve-type cenv target))
          pname (name property)]
      (if (str/starts-with? pname "-")
        (parse-field-access cenv target' target-type (subs pname 1))
        (or (parse-method-invocation cenv target' target-type pname maybe-args)
            (if (empty? maybe-args)
              (parse-field-access cenv target' target-type pname)
              (throw (ex-info (str "No such method: " pname) {:name pname}))))))))

(defn fold-aget [[_ arr index & indices :as expr]]
  (if (empty? indices)
    expr
    (recur (with-meta `(~'aget (~'aget ~arr ~index) ~@indices) (meta expr)))))

(defmethod parse-expr* 'alength [cenv [_ arr :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-expr cenv (with-meta `(.-length ~arr) (meta expr))))

(defmethod parse-expr* 'aget [cenv [_ arr index & indices :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if indices
    (parse-expr cenv (fold-aget expr))
    (let [cenv' (with-context cenv :expression)
          arr (parse-expr cenv' arr)
          _ (when-not (t/array-type? (:type arr))
              (error (format "array required, but %s found" (stringify-type (:type arr)))))
          index' (ensure-numeric-int cenv' (parse-expr cenv' index))]
      (-> {:op :array-access
           :type (t/element-type (:type arr))
           :array arr
           :index index'}
          (inherit-context cenv)))))

(defmethod parse-expr* 'aset [cenv [_ arr index & more :as expr]]
  (ensure-sufficient-arguments 3 expr :varargs? true)
  (if (next more)
    (let [indices (cons index (butlast more))
          form (with-meta
                 `(~'aset (~'aget ~arr ~@(butlast indices)) ~(last indices) ~(last more))
                 (meta expr))]
      (parse-expr cenv form))
    (let [cenv' (with-context cenv :expression)
          arr (parse-expr cenv' arr)
          _ (when-not (t/array-type? (:type arr))
              (error (format "array required, but %s found" (stringify-type (:type arr)))))
          elem-type (t/element-type (:type arr))
          index' (ensure-numeric-int cenv' (parse-expr cenv' index))
          expr' (ensure-type cenv' elem-type (parse-expr cenv' (first more)))]
      (-> {:op :array-update
           :type elem-type
           :array arr
           :index index'
           :expr expr'}
          (inherit-context cenv)))))
