(ns jise.parse
  (:refer-clojure :exclude [find-var])
  (:require [clojure.string :as str]
            [jise.error :as err :refer [error]]
            [jise.macroexpand :as mex]
            [jise.misc :as misc]
            [jise.simplify :as simp]
            [jise.type :as t])
  (:import [clojure.asm Type]
           [clojure.java.api Clojure]
           [java.lang.annotation Annotation ElementType Retention RetentionPolicy Target]))

(def ^:private ^:dynamic *active-labels* #{})

(def ^:const ^:private allowed-modifiers
  [:abstract :final :private :protected :public :static :synchronized :transient :volatile])

(defn- modifiers-of [[_ name :as form]]
  (merge (meta form) (meta name)))

(defn- access-flags [modifiers]
  (let [access (-> modifiers (select-keys allowed-modifiers) keys set)
        accessibility (filter #{:public :protected :private} access)]
    (case (count accessibility)
      0 (conj access :package)
      1 access
      (error (str "illegal combination of modifiers: " (str/join " and " accessibility))))))

(defn resolve-type [proto-cenv tag & {:keys [allow-vararg-param-type?]}]
  (try
    (t/tag->type proto-cenv tag
                 :allow-vararg-param-type? allow-vararg-param-type?)
    (catch Exception e
      (case (:cause (ex-data e))
        (:unresolved-type :invalid-vararg-param-type)
        (error (ex-message e) (dissoc (ex-data e) :cause))

        (error (str "cannot resolve type " (pr-str tag)) {:unresolved-type tag})))))

(defrecord AnnotationRecord [type values])

(defn- element-value-type [proto-cenv annotation-type name]
  (if-let [{:keys [return-type]} (->> (t/find-methods proto-cenv nil annotation-type name []
                                                      :throws-on-failure? false)
                                      first)]
    return-type
    (error (str "cannot find symbol: " name))))

(declare parse-annotation)

(defn- parse-element-value [proto-cenv t val]
  ;; the simplified value may be false (which is a valid constant value),
  ;; so handle it carefully to distinguish the value from nil
  (letfn [(simplify! [v]
            (if-some [v' (simp/simplify proto-cenv v)]
              v'
              (error "element value must be a constant expression")))]
    (if-some [val' (cond
                     (t/primitive-type? t)
                     (let [val (simplify! val)]
                       (if (= t/BOOLEAN t)
                         (when (boolean? val) val)
                         (when (number? val)
                           (cond (= t/BYTE t) (unchecked-byte val)
                                 (= t/SHORT t) (unchecked-short val)
                                 (= t/CHAR t) (unchecked-char val)
                                 (= t/INT t) (unchecked-int val)
                                 (= t/FLOAT t) (unchecked-float val)
                                 (= t/DOUBLE t) (unchecked-double val)))))

                     (= t t/STRING)
                     (let [val (simplify! val)]
                       (when (string? val) val))

                     (= t t/CLASS)
                     (when-let [t' (t/tag->type proto-cenv val)]
                       t')

                     (t/array-type? t)
                     (let [t' (t/element-type t)]
                       (if (vector? val)
                         (mapv (partial parse-element-value proto-cenv t') val)
                         [(parse-element-value proto-cenv t' val)]))

                     (t/super? proto-cenv (t/tag->type 'Enum) t)
                     (let [val' (try (eval val) (catch Throwable _))]
                       (when (instance? (t/type->class proto-cenv t) val')
                         val'))

                     (t/super? proto-cenv (t/tag->type 'java.lang.annotation.Annotation) t)
                     (when-let [[nested val'] (and (seq? val) val)]
                       (let [ann' (when (symbol? nested)
                                    (t/type->class proto-cenv (t/tag->type proto-cenv nested)))]
                         (when (and (class? ann') (= (t/class->type ann') t))
                           (let [{:keys [type values]} (parse-annotation proto-cenv ann' val')]
                             (->AnnotationRecord type values))))))]
      val'
      (let [vt (t/class->type (class val))]
        (err/error-on-incompatible-types t (or (t/unboxed-types vt) vt))))))

(defn- parse-annotation [proto-cenv ^Class ann value]
  (let [annotation-type (t/class->type ann)]
    (when (contains? (supers ann) Annotation)
      (let [vals (if (map? value)
                   (into {} (map (fn [[k v]]
                                   (let [kname (name k)
                                         et (element-value-type proto-cenv annotation-type kname)]
                                     [kname (parse-element-value proto-cenv et v)])))
                         value)
                   (let [et (element-value-type proto-cenv annotation-type "value")]
                     {"value" (parse-element-value proto-cenv et value)}))]
        (when-first [elem (remove (fn [^java.lang.reflect.Method m]
                                    (or (vals (.getName m))
                                        (not (nil? (.getDefaultValue m)))))
                                  (.getDeclaredMethods ann))]
          (error (format "@%s is missing a default value for the element '%s'"
                         (err/stringify-type annotation-type)
                         (.getName ^java.lang.reflect.Method elem))))
        {:type annotation-type
         :retention (or (some-> ^Retention (.getAnnotation ann Retention) (.value))
                        RetentionPolicy/RUNTIME)
         :target (some->> ^Target (.getAnnotation ann Target) (.value) set)
         :values vals}))))

(defn- parse-annotations [proto-cenv m]
  (reduce (fn [anns [k v]]
            (if (symbol? k)
              (let [ann (resolve k)]
                (if-let [ann' (and (class? ann) (parse-annotation proto-cenv ann v))]
                  (conj anns ann')
                  (do (binding [*out* *err*]
                        (println (str "Warning: " k " is not an annotation type, ignored")))
                      anns)))
              anns))
          []
          m))

(defn- check-annotation-target [target annotations]
  (when-first [ann (remove #(contains? (:target %) target) annotations)]
    (error (format "annotation type %s not applicable to this kind of declaration"
                   (err/stringify-type (:type ann))))))

(defn- parse-modifiers
  [proto-cenv {:keys [tag] :as modifiers} & {:keys [default-type allow-vararg-param?]}]
  (let [annotations (parse-annotations proto-cenv modifiers)]
    (cond-> {:type (if (nil? tag)
                     (or default-type t/OBJECT)
                     (resolve-type proto-cenv tag
                                   :allow-vararg-param-type? allow-vararg-param?))
             :access (access-flags modifiers)}
      (seq annotations)
      (assoc :annotations annotations))))

(defn- field-init-value [proto-cenv access [_ _ value :as field]]
  (and (:final access) (simp/simplify proto-cenv value)))

(defn- parse-field [proto-cenv [_ fname value :as field]]
  (let [has-init? (= (count field) 3)
        modifiers (modifiers-of field)
        {:keys [access type annotations]} (parse-modifiers proto-cenv modifiers)
        value' (when has-init?
                 (field-init-value proto-cenv access field))]
    (when (and value' (not (t/constant-value-compatible-with? type value')))
      (let [t (t/class->type (class value'))]
        (err/error-on-incompatible-types type (or (t/unboxed-types t) t))))
    (cond-> {:name (str fname)
             :type type
             :annotations annotations
             :access access}
      value' (assoc :value value')
      (and (:final access) (not has-init?)) (assoc :blank? (atom true)))))

(defn- context-of [{:keys [context]}]
  (if (:conditional context)
    (-> context (conj :expression) (disj :conditional))
    context))

(defn with-context [x context]
  (assoc x :context #{context}))

(defn inherit-context [x y & {:keys [return?]}]
  (assoc x :context
         (cond-> (context-of y)
           (not (nil? return?)) ((if return? conj disj) :return))))

(defn- first-meaningful-node [node]
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
      (if (empty? cs)
        src
        (-> (apply-conversions cs src)
            (inherit-context cenv)))
      (err/error-on-incompatible-types type (:type src)))))

(defn find-in-current-class [cenv & ks]
  (get-in cenv (into [:classes (:class-name cenv)] ks)))

(defn find-lname [cenv sym]
  (get (:lenv cenv) (name sym)))

(defn- find-var [cenv sym]
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

(def ^:private VAR_TYPE (Type/getType clojure.lang.Var))
(def ^:private IFN_TYPE (Type/getType clojure.lang.IFn))

(defn find-field [cenv class fname]
  (or (t/find-field cenv (:class-type cenv) class fname)
      (when (and (= class (:class-type cenv))
                 (get-in @(:vars cenv) [:fields fname]))
        {:class class :type VAR_TYPE :access #{:public :static}})))

(defn- expand-fn-invocation [cenv {:keys [var field-name]} [op & args]]
  (let [nargs (count args)
        cname (:class-name cenv)
        field (symbol (str \- field-name))
        sig (->> var meta :arglists
                 (filter (fn [^clojure.lang.APersistentVector sig]
                           (or (= (count sig) nargs)
                               (let [offset (.indexOf sig '&)]
                                 (and (>= offset 0)
                                      (>= nargs offset))))))
                 first)
        tag (or (:tag (meta op)) (:tag (meta sig)) (:tag (meta var)))]
    (cond->>
        (if-let [primc (some->> sig clojure.lang.Compiler$FnMethod/primInterface)]
          `(.invokePrim (jise.core/cast ~(symbol primc) (.deref (. ~cname ~field))) ~@args)
          `(.invoke (jise.core/cast clojure.lang.IFn (.deref (. ~cname ~field))) ~@args))
      tag
      (list 'jise.core/cast tag))))

(declare parse-expr parse-method-invocation)

(defn- parse-sugar [{:keys [class-type] :as cenv} [op :as expr]]
  (or (when-let [{:keys [type] :as x} (and (not (namespace op))
                                           (or (find-lname cenv op)
                                               (find-field cenv class-type (name op))))]
        (cond (t/array-type? type)
              (parse-expr cenv (with-meta `(jise.core/aget ~@expr) (meta expr)))

              (t/super? cenv IFN_TYPE type)
              (parse-expr cenv (with-meta `(.invoke ~op ~@(rest expr)) (meta expr)))

              :else
              (error (format "array or clojure.lang.IFn required, but %s found"
                             (err/stringify-type type)))))
      (when (t/get-methods cenv class-type class-type (str op))
        (parse-method-invocation cenv nil class-type (str op) (rest expr)))
      (when-let [{:keys [var] :as var-entry} (and (namespace op) (find-var cenv op))]
        (when-not (:macro (meta var))
          (let [form (expand-fn-invocation cenv var-entry expr)]
            (parse-expr cenv (with-meta form (meta expr))))))))

(defmulti parse-expr* (fn [cenv expr] (misc/fixup-ns (first expr))))
(defmethod parse-expr* :default [cenv expr]
  (let [expanded (mex/macroexpand cenv expr)]
    (if-not (identical? expanded expr)
      (parse-expr cenv expanded)
      (or (parse-sugar cenv expr)
          (error (str "cannot find symbol: " (first expr)) {:expr expr})))))

(defn- parse-symbol [cenv sym & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
  (if-let [tag (:tag (meta sym))]
    (parse-expr cenv `(jise.core/cast ~tag ~(vary-meta sym dissoc :tag)))
    (letfn [(parse-as-field [cenv callee]
              (parse-expr cenv (with-meta `(. ~callee ~(symbol (str \- (name sym)))) (meta sym))))
            (parse-this [cenv]
              (if (:static? cenv)
                (error "non-static variable this cannot be referenced from a static context")
                (-> {:op :local :type (:class-type cenv)
                     :local {:index 0 :access #{:final} :param? true}}
                    (inherit-context cenv))))
            (parse-super [cenv]
              (if (:static? cenv)
                (error "non-static variable super cannot be referenced from a static context")
                (-> {:op :super :type (find-in-current-class cenv :parent)}
                    (inherit-context cenv))))]
      (let [sym' (misc/resolve-ns sym)]
        (if-let [cname (namespace sym')]
          (if (= cname "jise.core")
            (case (misc/strip-jise-ns sym')
              this (parse-this cenv)
              super (parse-super cenv)
              (error (str "cannot find symbol: " sym)))
            (if-let [{:keys [field-name]} (find-var cenv sym)]
              (let [form `(.deref (. ~(:class-name cenv) ~(symbol (str \- field-name))))]
                (parse-expr cenv (with-meta form (meta sym))))
              (parse-as-field cenv (symbol cname))))
          (if-let [{:keys [type foreign?] :as local} (find-lname cenv sym)]
            (if foreign?
              (parse-as-field cenv 'jise.core/this)
              (inherit-context {:op :local :type type :local local} cenv))
            (case sym
              this (parse-this cenv)
              super (parse-super cenv)
              (if-let [f (find-field cenv (:class-type cenv) (name sym))]
                (let [callee (if (:static (:access f)) (:class-name cenv) 'jise.core/this)]
                  (parse-as-field cenv callee))
                (when throws-on-failure?
                  (error (str "cannot find symbol: " sym) {:variable sym}))))))))))

(defn- parse-seq [cenv expr]
  (if-let [tag (:tag (meta expr))]
    (parse-expr cenv `(jise.core/cast ~tag ~(vary-meta expr dissoc :tag)))
    (let [{:keys [tag line label]} (meta expr)
          cenv' (if label (inherit-context cenv cenv :return? false) cenv)
          expr' (if (symbol? (first expr))
                  (binding [*active-labels* (cond-> *active-labels* label (conj label))]
                    (err/with-line&column-of expr
                      (parse-expr* cenv' expr)))
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
      (-> {:op :literal :type t :value v}
          (inherit-context cenv))
      (error (str (pr-str v) " cannot be used as literal")))))

(defn parse-expr [{:keys [return-type] :as cenv} expr]
  (let [expr' (cond (symbol? expr) (parse-symbol cenv expr)
                    (seq? expr) (parse-seq cenv expr)
                    :else (parse-literal cenv expr))
        {:keys [context type]} expr']
    (if (and (:return context)
             (:expression context)
             (not= (or type t/VOID) return-type))
      (let [conv #(ensure-type cenv return-type %)]
        (if (= type t/VOID)
          ;; insert implicit (do ... nil)
          (-> {:op :do
               :type return-type
               :exprs [(with-context expr' :statement)
                       (conv (parse-literal cenv nil))]}
              (inherit-context cenv :return? false))
          (conv expr')))
      expr')))

(defn parse-exprs [cenv body]
  (let [cenv' (with-context cenv :statement)
        exprs' (mapv parse-expr (repeat cenv') (butlast body))
        last' (parse-expr cenv (last body))]
    (-> {:op :do :type (:type last')
         :exprs (conj exprs' last')}
        (inherit-context cenv :return? false))))

(defn- parse-name [proto-cenv name param? & {:keys [default-type allow-vararg-param?]}]
  (let [{:keys [access type annotations]} (parse-modifiers proto-cenv (meta name)
                                                           :default-type default-type
                                                           :allow-vararg-param? allow-vararg-param?)]
    (check-annotation-target (if param?
                               ElementType/PARAMETER
                               ElementType/LOCAL_VARIABLE)
                             annotations)
    {:name name
     :type type
     :access access
     :annotations annotations}))

(defn- next-index [cenv type]
  (first (swap-vals! (:next-index cenv) + (t/type-category type))))

(defn- parse-binding [cenv lname init param? allow-vararg-param?]
  (let [init' (when-not param?
                (parse-expr (with-context cenv :expression) init))
        lname' (parse-name cenv lname param?
                           :default-type (:type init')
                           :allow-vararg-param? allow-vararg-param?)
        init' (when init' (ensure-type cenv (:type lname') init' :context :casting))]
    (-> lname'
        (update :name name)
        (assoc :index (next-index cenv (:type lname')))
        (cond-> init' (assoc :init init')))))

(defn- parse-bindings [cenv bindings & {:keys [params?]}]
  (loop [[lname init & bindings] bindings
         cenv' (with-context cenv :expression)
         allow-vararg-param? false
         ret []]
    (if lname
      (if (= lname '&)
        (if params?
          (recur bindings cenv' true ret)
          (error "varargs parameter is only allowed in method signature"))
        (if (and allow-vararg-param? (seq bindings))
          (error "varargs parameter must be the last parameter")
          (let [b (parse-binding cenv' lname init params?
                                 (and allow-vararg-param? (empty? bindings)))
                cenv' (assoc-in cenv' [:lenv (:name b)]
                                (cond-> b params? (assoc :param? true)))]
            (when (and allow-vararg-param? (not (t/array-type? (:type b))))
              (error "varargs parameter must be of array type"))
            (recur bindings cenv' allow-vararg-param? (conj ret b)))))
      [(inherit-context cenv' cenv) ret])))

(declare parse-ctor-invocation)

(defn- inject-ctor-invocation [cenv body]
  (or (when-let [node (first-meaningful-node body)]
        (when-not (= (:op node) :ctor-invocation)
          (let [non-empty? (not (and (= (:op node) :null) (:return (:context node))))
                cenv' (cond-> cenv non-empty? (with-context :statement))]
            (-> {:op :do :type (:type body)
                 :exprs (cond-> [(parse-ctor-invocation cenv' '(jise.core/super))]
                          ;; omit entire body when body is empty `do`
                          non-empty? (conj body))}
                (inherit-context cenv :return? false)))))
      body))

(defn- parse-exceptions [cenv exceptions]
  (when exceptions
    (mapv (fn [e]
            (let [t (resolve-type cenv e)]
              (if (t/super? cenv t/THROWABLE t)
                t
                (error (err/error-message-on-incompatible-types t/THROWABLE t)))))
          exceptions)))

(defn- parse-method [cenv ctor? [_ mname args & body :as method]]
  (err/with-line&column-of method
    (let [modifiers (modifiers-of method)
          {:keys [access type annotations]} (parse-modifiers cenv modifiers :default-type t/VOID)
          _ (when (and (:abstract access) body)
              (error "abstract methods cannot have a body"))
          _ (check-annotation-target ElementType/METHOD annotations)
          static? (boolean (:static access))
          init-index (if static? 0 1)
          [cenv' args'] (parse-bindings (assoc cenv
                                               :lenv (:enclosing-env cenv)
                                               :next-index (atom init-index))
                                        (interleave args (repeat nil))
                                        :params? true)
          exceptions (parse-exceptions cenv (:throws modifiers))
          return-type (if ctor? t/VOID type)
          context (if (= return-type t/VOID) :statement :expression)
          this-name (or (:jise.core/this-name (meta method)) (:jise.core/this-name cenv))
          cenv' (cond-> (assoc cenv'
                               :return-type return-type
                               :context #{context :tail :return}
                               :static? static?)
                  exceptions
                  (assoc :exceptions (set exceptions))

                  (and (not static?) this-name)
                  (assoc-in [:lenv (name this-name)]
                            {:index 0 :type (:class-type cenv)
                             :access #{:final} :param? true}))
          body' (cond->> (parse-exprs cenv' body)
                  ctor? (inject-ctor-invocation cenv'))]
      (cond-> {:return-type return-type
               :args args'
               :annotations annotations
               :access access
               :body body'}
        ctor? (assoc :ctor? ctor?)
        (not ctor?) (assoc :name (str mname))
        (some #{'&} args) (update :access conj :varargs)
        exceptions (assoc :exceptions exceptions)))))

(defn- parse-supers [proto-cenv [maybe-supers & body]]
  (let [supers (when (vector? maybe-supers) maybe-supers)
        supers' (map (partial resolve-type proto-cenv) supers)
        {parents false
         interfaces true} (group-by #(.isInterface (t/type->class proto-cenv %)) supers')]
    {:parents (cond-> parents (empty? parents) (conj t/OBJECT))
     :interfaces interfaces
     :body (cond->> body (nil? supers) (cons maybe-supers))}))

(defn- class-alias [cname]
  (symbol (str/replace cname #"^.*\.([^.]+)" "$1")))

(defn- group-decls [cname decls]
  (let [alias (class-alias cname)
        dest-key (fn [{:keys [static]}]
                   (if static :static :non-static))]
    (loop [decls decls
           ret {:ctors [], :methods [], :static [] :non-static []}]
      (if (empty? decls)
        ret
        (let [[decl & decls] decls]
          (if (seq? decl)
            (case (misc/fixup-ns (first decl))
              def (recur decls (update ret (dest-key (modifiers-of decl)) conj decl))
              defm (let [[_ name] decl
                         key (if (= name alias) :ctors :methods)]
                     (recur decls (update ret key conj decl)))
              do (let [decls' (concat (cond->> (rest decl)
                                        (:static (meta decl))
                                        (map (fn [decl]
                                               (if (instance? clojure.lang.IObj decl)
                                                 (vary-meta decl assoc :static true)
                                                 decl))))
                                      decls)]
                   (recur decls' ret))
              (let [[op & args] decl
                    v (when (symbol? op) (resolve op))
                    [decls ret] (if (and v (:macro (meta v)))
                                  [(cons (mex/macroexpand {} decl) decls) ret]
                                  [decls (update ret (dest-key (meta decl)) conj decl)])]
                (recur decls ret)))
            (recur decls ret)))))))

(defn- parse-method-signature [proto-cenv [_ name args :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [type access]} (parse-modifiers proto-cenv modifiers :default-type t/VOID)
        varargs? (volatile! false)
        param-types (reduce (fn [pts [prev arg]]
                              (if (= arg '&)
                                pts
                                (let [vararg-param? (= prev '&)]
                                  (when vararg-param? (vreset! varargs? true))
                                  (->> (parse-name proto-cenv arg true :allow-vararg-param? vararg-param?)
                                       :type
                                       (conj pts)))))
                            []
                            (partition 2 1 (cons nil args)))]
    {:access (cond-> access @varargs? (conj :varargs))
     :return-type type
     :param-types param-types}))

(defn- build-class-entry [proto-cenv cname parent interfaces {:keys [ctors methods]}]
  (let [ctors' (reduce (fn [cs [_ _ args :as ctor]]
                         (err/with-line&column-of ctor
                           (let [access (access-flags (modifiers-of ctor))
                                 param-types (mapv #(:type (parse-name proto-cenv % true)) args)]
                             (conj cs {:access access :param-types param-types}))))
                       [] ctors)
        methods' (reduce (fn [m [_ name :as method]]
                           (err/with-line&column-of method
                             (let [method' (parse-method-signature proto-cenv method)]
                               (update m (str name) (fnil conj []) method'))))
                         {} methods)]
    {:parent parent :interfaces (set interfaces) :ctors ctors' :methods methods'}))

(defn- convert-def-to-set [cname [_ name init :as def]]
  (let [static? (:static (modifiers-of def))]
    `(jise.core/set! (. ~(if static? cname 'jise.core/this)
                        ~(symbol (str \- name)))
                     ~init)))

(defn- parse-fields [cenv fields]
  (let [cname (:class-name cenv)]
    (reduce (fn [[cenv initializer] field]
              (let [[_ fname init] field
                    {:keys [static] :as modifiers} (modifiers-of field)
                    cenv' (cond-> cenv static (assoc :static? true))
                    field' (err/with-line&column-of field
                             (parse-field cenv' field))
                    field' (cond-> field'
                             (and (:final (:access field'))
                                  (not (contains? field' :value)))
                             (assoc :blank? (atom true)))]
                [(assoc-in cenv [:classes cname :fields (:name field')] field')
                 (if (and init (not (contains? field' :value)))
                   (conj initializer (convert-def-to-set cname field))
                   initializer)]))
            [cenv []]
            fields)))

(defn- build-cenv
  [proto-cenv cname modifiers parent interfaces {:keys [static non-static] :as decls}]
  (let [[cenv' stinit] (parse-fields proto-cenv static)
        [cenv' init] (parse-fields cenv' non-static)
        init' (when (seq init) `(jise.core/do ~@init))
        entry (build-class-entry cenv' cname parent interfaces decls)]
    (-> (update-in cenv' [:classes cname] merge entry)
        (assoc-in [:classes cname :initializer] init')
        (assoc :static-initializer stinit
               :vars (atom {:var->entry {} :fields #{}}))
        (cond->
            (:jise.core/this-name modifiers)
          (assoc :jise.core/this-name (:jise.core/this-name modifiers))))))

(defn- check-supers [cenv [parent :as parents] interfaces]
  (doseq [c (concat parents interfaces)]
    (when (or (t/primitive-type? c) (t/array-type? c))
      (error (str "unexpected type\n"
                  "  required: class\n"
                  "  found: " (err/stringify-type c)))))
  (when (> (count parents) 1)
    (error (str "cannot inherit from more than one classes: "
                (->> parents (map err/stringify-type) (str/join ", ")))))
  (when (t/final-type? cenv parent)
    (error (str "cannot inherit from final " (err/stringify-type parent)))))

(defn- check-blank-final-initialization [cenv]
  (let [fields (find-in-current-class cenv :fields)]
    (when-let [field (some (fn [[_ field]]
                             (when (and (:final (:access field))
                                        (some-> (:blank? field) deref))
                               field))
                           fields)]
      (error (str "variable " (:name field) " might not have been initialized")))))

(defn- synthesize-static-fields [{:keys [vars]}]
  (reduce (fn [fields [_ {:keys [field-name var-name]}]]
            (conj fields
                  `(jise.core/def
                     ~(with-meta
                        field-name
                        {:tag 'clojure.lang.Var :private true :static true})
                     (jise.core/cast clojure.lang.Var (Clojure/var ~(str var-name))))))
          []
          (:var->entry @vars)))

(defn- parse-static-initializer [cenv]
  (let [{:keys [static-initializer class-name]} cenv
        additional-static-fields (synthesize-static-fields cenv)
        [cenv' additional-stinit] (parse-fields cenv additional-static-fields)]
    [cenv'
     (when-let [init (seq (concat static-initializer additional-stinit))]
       (let [m `^:static (jise.core/defm ~'<clinit> [] ~@init)]
         (-> (parse-method cenv false m)
             (assoc :static-initializer? true))))]))

(defn- synthesize-non-static-fields [{:keys [enclosing-env]}]
  (reduce (fn [fields [name {:keys [type used?]}]]
            (if @used?
              (let [type' (t/type->tag type)]
                (conj fields
                      (with-meta
                        `(jise.core/def ~(symbol name))
                        {:tag type' :private true
                         :final true :synthetic true})))
              fields))
          []
          enclosing-env))

(defn- parse-ctors [cenv ctors]
  (letfn [(names-for-synthetic-fields [fields]
            (mapv (fn [[_ name :as field]]
                    (vary-meta name assoc :tag (:tag (meta field))))
                  fields))
          (add-params-for-synthetic-fields [names ctors]
            (for [[_ name params & body :as ctor] ctors]
              (with-meta
                `(jise.core/defm ~name ~(into names params)
                   ~@body)
                (meta ctor))))]
    (let [synthetic-fields (synthesize-non-static-fields cenv)
          names (names-for-synthetic-fields synthetic-fields)
          [cenv' _] (parse-fields cenv synthetic-fields)
          cenv' (assoc cenv' :synthetic-fields names)
          ctors' (->> ctors
                      (add-params-for-synthetic-fields names)
                      (mapv (partial parse-method cenv' true)))
          synthetic-fields' (synthesize-non-static-fields cenv')]
      ;; when ctors yield additional synthetic fields, parse them again
      (if (= (count synthetic-fields) (count synthetic-fields'))
        [cenv' ctors']
        (let [names' (names-for-synthetic-fields synthetic-fields')
              cenv' (assoc cenv' :synthetic-fields names')]
          [cenv'
           (->> ctors
                (add-params-for-synthetic-fields names')
                (mapv (partial parse-method cenv' true)))])))))

(defn parse-class
  ([class] (parse-class {} class))
  ([enclosing-env [_ cname & body :as class]]
   (err/with-line&column-of class
     (let [alias (class-alias cname)
           proto-cenv (as-> {:class-name cname :classes {cname {}}
                             :aliases (cond-> {} (not= cname alias) (assoc alias cname))
                             :enclosing-env enclosing-env}
                          proto-cenv
                        (assoc proto-cenv :class-type (t/find-in-cenv proto-cenv cname)))
           modifiers (modifiers-of class)
           {:keys [parents interfaces body]} (parse-supers proto-cenv body)
           _ (check-supers proto-cenv parents interfaces)
           parent (first parents)
           proto-cenv (assoc proto-cenv :parent parent :interfaces interfaces)
           decls (group-decls alias body)
           cenv (build-cenv proto-cenv cname modifiers parent interfaces decls)
           annotations (parse-annotations cenv modifiers)
           _ (check-annotation-target ElementType/TYPE annotations)
           methods' (mapv (partial parse-method cenv false) (:methods decls))
           [cenv' stinit] (parse-static-initializer cenv)
           ctors (if-let [ctors (seq (:ctors decls))]
                   ctors
                   [(with-meta `(jise.core/defm ~alias [] (jise.core/super))
                      (select-keys modifiers [:public :protected :private]))])
           [cenv' ctors'] (parse-ctors cenv ctors)
           _ (check-blank-final-initialization cenv')]
       {:name (str/replace (str cname) \. \/)
        :access (access-flags modifiers)
        :annotations annotations
        :parent parent
        :interfaces interfaces
        :static-initializer stinit
        :ctors ctors'
        :methods methods'
        :fields (vals (find-in-current-class cenv' :fields))}))))

(defn- ensure-numeric [cenv x op-name]
  (if-let [cs (t/unary-numeric-promotion (:type x))]
    (-> (apply-conversions cs x)
        (inherit-context cenv))
    (err/error-on-bad-operand-type op-name (:type x))))

(defn- parse-unary-op [cenv [op-name x] op]
  (let [cenv' (with-context cenv :expression)
        x' (ensure-numeric cenv' (parse-expr cenv' x) op-name)]
    (-> {:op op
         :type (:type x')
         :operand x'}
        (inherit-context cenv))))

(defn- parse-binary-op
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
     (err/error-on-bad-operand-types op-name (:type lhs) (:type rhs)))))

(defn- parse-arithmetic [cenv expr op]
  (let [{:keys [lhs] :as ret} (parse-binary-op cenv expr op)]
    (assoc ret :type (:type lhs))))

(defn- coerce-to-primitive [cenv [op x]]
  (let [cenv' (with-context cenv :expression)]
    (ensure-numeric cenv (parse-expr cenv' x) op)))

(defn- fold-binary-op [[op x y & more :as expr]]
  (if more
    (recur (with-meta `(~op (~op ~x ~y) ~@more) (meta expr)))
    expr))

(defn ensure-sufficient-arguments [num [op-name & args] & {:keys [varargs?]}]
  (let [nargs (count args)]
    (when-not (if varargs?
                (>= nargs num)
                (= nargs num))
      (err/error-on-missing-arguments op-name num varargs?))))

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
    1 (parse-expr cenv (with-meta `(jise.core// 1 ~(first args)) (meta expr)))
    2 (parse-arithmetic cenv expr :div)
    (parse-expr cenv (fold-binary-op expr))))

(defmethod parse-expr* 'rem [cenv expr]
  (ensure-sufficient-arguments 2 expr)
  (parse-arithmetic cenv expr :rem))

(defn- unbox-if-needed [x]
  (if-let [c (t/unboxing-conversion (:type x))]
    (apply-conversions [c] x)
    x))

(defn- parse-logical [cenv [op-name x y] op]
  (let [cenv' (with-context cenv :expression)
        {t1 :type :as lhs} (parse-expr cenv' x)
        {t2 :type :as rhs} (parse-expr cenv' y)]
    (cond (and (t/boolean-type? t1) (t/boolean-type? t2))
          (-> {:op op
               :type t/BOOLEAN
               :lhs (unbox-if-needed lhs)
               :rhs (unbox-if-needed rhs)}
              (inherit-context cenv))

          (and (t/convertible-to-integral? t1) (t/convertible-to-integral? t2))
          (let [{:keys [lhs] :as ret} (parse-binary-op cenv lhs rhs op op-name)]
            (assoc ret :type (:type lhs)))

          :else (err/error-on-bad-operand-types op-name t1 t2))))

(defmethod parse-expr* '& [cenv expr]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (parse-logical cenv (fold-binary-op expr) :bitwise-and))

(defmethod parse-expr* '| [cenv expr]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (parse-logical cenv (fold-binary-op expr) :bitwise-or))

(defmethod parse-expr* 'xor [cenv expr]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (parse-logical cenv (fold-binary-op expr) :bitwise-xor))

(defmethod parse-expr* '! [cenv [_ operand :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-expr cenv (with-meta `(jise.core/xor ~operand -1) (meta expr))))

(defn- parse-shift [cenv [op-name x y :as expr] op]
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
      (err/error-on-bad-operand-types op-name (:type lhs) (:type rhs)))
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

(defn- parse-equal [cenv {t1 :type :as lhs} {t2 :type :as rhs} op op-name]
  (if (or (t/primitive-type? t1) (t/primitive-type? t2))
    (cond (and (t/convertible-to-numeric? t1)
               (t/convertible-to-numeric? t2))
          (-> (parse-binary-op cenv lhs rhs op op-name)
              (assoc :type t/BOOLEAN))

          (and (t/boolean-type? t1) (t/boolean-type? t2))
          {:op op
           :type t/BOOLEAN
           :lhs (unbox-if-needed lhs)
           :rhs (unbox-if-needed rhs)}

          :else (err/error-on-bad-operand-types op-name t1 t2))
    (if-let [[lhs' rhs'] (or (when-let [cs (t/casting-conversion cenv t1 t2)]
                               [(apply-conversions cs lhs) rhs])
                             (when-let [cs (t/casting-conversion cenv t2 t1)]
                               [lhs (apply-conversions cs rhs)]))]
      {:op op
       :type t/BOOLEAN
       :lhs lhs
       :rhs rhs}
      (err/error-on-bad-operand-types op-name t1 t2))))

(defn- conditional-context? [cenv]
  (:conditional (:context cenv)))

(defn- fold-comparison [[op & args :as expr]]
  (with-meta
    `(jise.core/and ~@(map (fn [[x y]] `(~op ~x ~y)) (partition 2 1 args)))
    (meta expr)))

(defn- parse-comparison
  ([cenv [op-name x y & more :as expr] op]
   (if (conditional-context? cenv)
     (if more
       (parse-expr cenv (fold-comparison expr))
       (let [cenv' (with-context cenv :expression)
             x' (parse-expr cenv' x)
             y' (parse-expr cenv' y)]
         (parse-comparison cenv' x' y' op op-name)))
     (parse-expr cenv `(jise.core/if ~expr true false))))
  ([cenv lhs rhs op op-name]
   (if (#{:eq :ne} op)
     (parse-equal cenv lhs rhs op op-name)
     (-> (parse-binary-op cenv lhs rhs op op-name)
         (assoc :type t/BOOLEAN)))))

(defn- parse-cmp-0 [cenv x y op default-op default-op-name default]
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

(defn- parse-eq-null [cenv x y op op-name default]
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
            (err/error-on-bad-operand-types op-name t1 t2)))))))

(defmethod parse-expr* '== [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when (and (nil? more) (conditional-context? cenv))
        (cond (or (= x 0) (= y 0)) (parse-cmp-0 cenv x y :eq-0 :eq '== true)
              (or (nil? x) (nil? y)) (parse-eq-null cenv x y :eq-null '== true)))
      (parse-comparison cenv expr :eq)))

(defmethod parse-expr* '!= [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if more
    (parse-expr cenv (with-meta `(jise.core/not (jise.core/== ~x ~y ~@more)) (meta expr)))
    (or (when (conditional-context? cenv)
          (cond (or (= x 0) (= y 0)) (parse-cmp-0 cenv x y :ne-0 :ne '!= false)
                (or (nil? x) (nil? y)) (parse-eq-null cenv x y :ne-null '!= false)))
        (parse-comparison cenv expr :ne))))

(defmethod parse-expr* '< [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when (and (nil? more) (conditional-context? cenv))
        (cond (= x 0) (parse-cmp-0 cenv x y :gt-0 :lt '< false)
              (= y 0) (parse-cmp-0 cenv x y :lt-0 :lt '< false)))
      (parse-comparison cenv expr :lt)))

(defmethod parse-expr* '> [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when (and (nil? more) (conditional-context? cenv))
        (cond (= x 0) (parse-cmp-0 cenv x y :lt-0 :gt '> false)
              (= y 0) (parse-cmp-0 cenv x y :gt-0 :gt '> false)))
      (parse-comparison cenv expr :gt)))

(defmethod parse-expr* '<= [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when (and (nil? more) (conditional-context? cenv))
        (cond (= x 0) (parse-cmp-0 cenv x y :ge-0 :le '<= true)
              (= y 0) (parse-cmp-0 cenv x y :le-0 :le '<= true)))
      (parse-comparison cenv expr :le)))

(defmethod parse-expr* '>= [cenv [_ x y & more :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (or (when (and (nil? more) (or (= x 0) (= y 0))
                 (conditional-context? cenv))
        (cond (= x 0) (parse-cmp-0 cenv x y :le-0 :ge '>= true)
              (= y 0) (parse-cmp-0 cenv x y :ge-0 :ge '>= true)))
      (parse-comparison cenv expr :ge)))

(defmethod parse-expr* 'and [cenv [_ & exprs :as expr]]
  (if (conditional-context? cenv)
    (case (count exprs)
      0 (parse-literal cenv true)
      1 (parse-expr cenv (first exprs))
      (let [exprs' (mapv #(unbox-if-needed (parse-expr cenv %)) exprs)]
        (when-let [[e1 e2] (some (fn [[e1 e2]]
                                   (when-not (and (= (:type e1) t/BOOLEAN)
                                                  (= (:type e2) t/BOOLEAN))
                                     [e1 e2]))
                                 (partition 2 1 exprs'))]
          (err/error-on-bad-operand-types 'and (:type e1) (:type e2)))
        {:op :and
         :type t/BOOLEAN
         :exprs exprs'}))
    (parse-expr cenv `(jise.core/if ~expr true false))))

(defn- negate-expr [{:keys [op] :as expr}]
  (case op
    :not (:expr expr)
    :and {:op :or
          :type t/BOOLEAN
          :exprs (butlast (:exprs expr))
          :expr (negate-expr (last (:exprs expr)))}
    :or {:op :and
         :type t/BOOLEAN
         :exprs (conj (vec (:exprs expr)) (negate-expr (:expr expr)))}
    {:op :not
     :type t/BOOLEAN
     :expr expr}))

(defmethod parse-expr* 'or [cenv [_ & exprs :as expr]]
  (if (conditional-context? cenv)
    (case (count exprs)
      0 (parse-literal cenv false)
      1 (parse-expr cenv (first exprs))
      (let [exprs' (mapv #(unbox-if-needed (parse-expr cenv %)) exprs)]
        (when-let [[e1 e2] (some (fn [[e1 e2]]
                                   (when-not (and (= (:type e1) t/BOOLEAN)
                                                  (= (:type e2) t/BOOLEAN))
                                     [e1 e2]))
                                 (partition 2 1 exprs'))]
          (err/error-on-bad-operand-types 'and (:type e1) (:type e2)))
        {:op :or
         :type t/BOOLEAN
         :exprs (mapv negate-expr (butlast exprs'))
         :expr (last exprs')}))
    (parse-expr cenv `(jise.core/if ~expr true false))))

(defmethod parse-expr* 'not [cenv [_ operand :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (if (conditional-context? cenv)
    (let [{:keys [type] :as operand'} (parse-expr cenv operand)]
      (if (t/boolean-type? type)
        (negate-expr (unbox-if-needed operand'))
        (err/error-on-bad-operand-type 'not type)))
    (parse-expr cenv `(jise.core/if ~expr true false))))

(defn- parse-cast [cenv type x]
  (let [cenv' (with-context cenv :expression)]
    (-> (ensure-type cenv type (parse-expr cenv' x) :context :casting)
        (inherit-context cenv))))

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
  (parse-expr cenv (with-meta `(jise.core/== ~arg nil) (meta expr))))

(defmethod parse-expr* `nil? [cenv [_ arg :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (parse-expr cenv (with-meta `(jise.core/== ~arg nil) (meta expr))))

(defmethod parse-expr* 'str [cenv [_ & args :as expr]]
  (if (every? string? args)
    (parse-expr cenv (apply str args))
    (let [form `(-> (jise.core/new StringBuilder)
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
      (err/error-on-incompatible-types c' (:type x')))
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
  (parse-expr cenv (with-meta `(jise.core/let* ~@(rest expr)) (meta expr))))

(defn- parse-field-update [cenv {:keys [field] :as lhs} rhs]
  (cond (and (:final (:access field))
             (if-let [blank? (:blank? field)]
               (not @blank?)
               true))
        (error (str "cannot assign a value to final variable " (:name field)))

        :else
        (do (some-> (:blank? field) (reset! false))
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

      (if (symbol? target)
        (let [local (:local lhs)]
          (if (:final (:access local))
            (if (:param? local)
              (error (str "final parameter " target " may not be assigned"))
              (error (str "cannot assign a value to final variable " target)))
            (-> {:op :assignment
                 :type (:type lhs)
                 :lhs lhs
                 :rhs rhs}
                (inherit-context cenv))))
        (error (str "unexpected type\n"
                    "  required: variable\n"
                    "  found:    value"))))))

(defn- parse-increment [cenv target by max-value op-name dec?]
  (let [{:keys [type] :as target'} (parse-expr (with-context cenv :expression) target)
        by (or by 1)]
    (when-not (t/numeric-type? type)
      (err/error-on-bad-operand-type op-name type))
    (if (and (= (:op target') :local)
             (or (= type t/INT)
                 (when-let [{:keys [to]} (t/widening-primitive-conversion type t/INT)]
                   (= to t/INT)))
             (int? by)
             (<= 0 (Math/abs (long by)) max-value))
      (let [local (:local target')]
        (if (:final (:access local))
          (if (:param? local)
            (error (str "final parameter " target " may not be assigned"))
            (error (str "cannot assign a value to final variable " target)))
          (-> {:op :increment, :target target', :type type, :by (cond-> by dec? -)}
              (inherit-context cenv))))
      (parse-expr cenv `(jise.core/set! ~target (~(if dec? 'jise.core/- 'jise.core/+) ~target ~by))))))

(defmethod parse-expr* 'inc! [cenv [_ target by]]
  (parse-increment cenv target by Byte/MAX_VALUE 'inc! false))

(defmethod parse-expr* 'dec! [cenv [_ target by]]
  (parse-increment cenv target by (- Byte/MIN_VALUE) 'dec! true))

(defn- convert-operand-types-for-conditional [cenv then else]
  (if (nil? else)
    [then else]
    (let [t1 (:type then), t2 (:type else)]
      (cond (= t1 t2) [then else]

            (and (t/boolean-type? t1) (t/boolean-type? t2))
            [(unbox-if-needed then) (unbox-if-needed else)]

            (and (t/convertible-to-numeric? t1)
                 (t/convertible-to-numeric? t2))
            (let [[c1 c2] (t/binary-numeric-promotion t1 t2)]
              [(apply-conversions c1 then)
               (apply-conversions c2 else)])

            (nil? t1)
            (let [c (t/widening-reference-conversion cenv t1 t2)]
              [(apply-conversions [c] then) else])

            (nil? t2)
            (let [c (t/widening-reference-conversion cenv t2 t1)]
              [then (apply-conversions [c] else)])

            ;; TODO: a reference conditional expression may be a poly
            ;; expression, and in that case, the type of expression should be
            ;; determined considering the "target type" (i.e. the expected type
            ;; from the context in which the expression appears)
            :else [then else]))))

(defmethod parse-expr* 'if [cenv [_ test then else]]
  (cond (true? test) (parse-expr cenv then)
        (false? test) (parse-expr cenv else)
        :else
        (let [context (context-of cenv)
              test' (as-> (parse-expr (with-context cenv :conditional) test) test'
                      (ensure-type (with-context cenv :expression)
                                   t/BOOLEAN test' :context :casting))
              [then' else'] (if (:statement context)
                              [(parse-expr cenv then)
                               (some->> else (parse-expr cenv))]
                              (convert-operand-types-for-conditional cenv
                               (parse-expr cenv then)
                               (parse-expr cenv else)))
              node {:op :if, :type (:type then'), :test test', :then then'}]
          (if else'
            (-> node
                (assoc :else else')
                (inherit-context cenv :return? false))
            (inherit-context node cenv)))))

(defn- parse-case-clause [cenv type sym [ks expr]]
  (let [ks (if (seq? ks) (vec ks) [ks])
        ks' (mapv (fn [k]
                    (or (when-let [k' (simp/simplify cenv k)]
                          (cond (t/convertible-to-integral? type)
                                (when (and (int? k')
                                           (<= Integer/MIN_VALUE k' Integer/MAX_VALUE))
                                  k')
                                (= type t/STRING)
                                (when (string? k')
                                  (.hashCode ^String k'))
                                :else nil))
                        (error (str k " cannot be converted to " (err/stringify-type type)))))
                  ks)
        guard (when (= type t/STRING)
                (->> (if (> (count ks) 1)
                       `(jise.core/or ~@(map (fn [k] `(jise.core/= ~sym ~k)) ks))
                       `(jise.core/= ~sym ~(first ks)))
                     (parse-expr (with-context cenv :conditional))))
        expr' (parse-expr cenv expr)]
    (cond-> {:keys ks' :type (:type expr') :body expr'}
      guard (assoc :guard guard))))

(defmethod parse-expr* 'case [cenv [_ test & clauses :as expr]]
  (if-let [l (and (symbol? test) (find-lname cenv test))]
    (let [default (when (odd? (count clauses))
                    (last clauses))
          context (context-of cenv)
          clauses' (->> (partition 2 clauses)
                        (mapv (partial parse-case-clause cenv (:type l) test)))
          default' (if (some? default)
                     (parse-expr cenv default)
                     (when (:expression context)
                       (parse-literal cenv nil)))
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
          form `(jise.core/let [~h ~test]
                  ~(with-meta
                     `(jise.core/case ~h ~@clauses)
                     (meta expr)))]
      (parse-expr cenv form))))

(defn- extract-label [expr]
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

(defn- parse-enhanced-for-loop [cenv [_ args & body :as form]]
  (let [[lname expr] args
        {:keys [type]} (parse-expr (with-context cenv :expression) expr)
        form' (cond (t/array-type? type)
                    `(jise.core/let [array# ~expr
                                     len# (.-length array#)
                                     ~lname (jise.core/aget array# 0)]
                       (jise.core/for [i# 0 (jise.core/< i# len#) (jise.core/inc! i#)]
                         (jise.core/set! ~lname (jise.core/aget array# i#))
                         ~@body))
                    (t/super? cenv (t/class->type Iterable) type)
                    `(jise.core/for [i# (.iterator ~expr) (.hasNext i#) nil]
                       (jise.core/let [~lname (.next i#)]
                         ~@body))
                    :else (error (str "for-each not applicable to expression type\n"
                                      "  required: array or Iterable\n"
                                      "  found: " (err/stringify-type type))))]
    (parse-expr cenv (with-meta form' (meta form)))))

(defmethod parse-expr* 'for [cenv [_ args & body :as form]]
  (let [nargs (count args)]
    (case nargs
      (0 1) (error "malformed for-loop")
      2 (parse-enhanced-for-loop cenv form)
      (let [[init bindings] (if (= nargs 3)
                              [(first args) nil]
                              [nil (drop-last 2 args)])
            _ (when-not (even? (count bindings))
                (error "malformed for-loop"))
            init' (some->> init (parse-expr (with-context cenv :statement)))
            [cond step] (take-last 2 args)
            [cenv' bindings'] (parse-bindings cenv bindings)
            cond' (as-> (parse-expr (with-context cenv' :conditional) cond) cond'
                    (ensure-type (with-context cenv :expression)
                                 t/BOOLEAN cond' :context :casting))
            label (extract-label form)
            node (-> {:op :for
                      :cond cond'
                      :step (parse-expr (with-context cenv' :statement) step)
                      :body (parse-exprs (with-context cenv' :statement) body)}
                     (inherit-context cenv)
                     (cond-> label (assoc :label label)))]
        (-> (if bindings
              {:op :let :bindings bindings' :body node}
              {:op :do :exprs (cond->> [node] init' (cons init'))})
            (inherit-context cenv :return? false))))))

(defn- split-with-ops [ops forms]
  (split-with (fn [x]
                (not (and (seq? x)
                          (let [op (first x)]
                            (and (symbol? op)
                                 (ops (misc/fixup-ns op)))))))
              forms))

(defn- parse-catch-clause [cenv [_ class lname & body]]
  (let [class' (resolve-type cenv class)
        _ (when-not (t/super? cenv t/THROWABLE class')
            (err/error-on-incompatible-types t/THROWABLE class'))
        [cenv' [b]] (parse-bindings cenv [(vary-meta lname assoc :tag class) nil] :params? true)
        body' (parse-exprs cenv' body)]
    {:type (:type body')
     :local b
     :body body'}))

(defmethod parse-expr* 'try [cenv [_ & body]]
  (let [[body clauses] (split-with-ops '#{catch finally} body)
        [catch-clauses finally-clauses] (split-with-ops '#{finally} clauses)
        finally-clause (nfirst finally-clauses)
        cenv' (with-context cenv (:expression (context-of cenv) :statement))
        body' (parse-exprs cenv' body)
        catch-clauses' (mapv (partial parse-catch-clause cenv') catch-clauses)
        finally-clause' (some->> finally-clause (parse-exprs (with-context cenv :statement)))]
    (-> {:op :try
         :type (:type body')
         :body body'
         :catch-clauses catch-clauses'
         :finally-clause finally-clause'}
        (inherit-context cenv))))

(defn- ensure-existing-label [label]
  (when (and label (not (contains? *active-labels* label)))
    (error (str "undefined label: " label))))

(defmethod parse-expr* 'continue [cenv [_ label]]
  (ensure-existing-label label)
  (-> {:op :continue}
      (inherit-context cenv :return? false)
      (cond-> label (assoc :label label))))

(defmethod parse-expr* 'break [cenv [_ label]]
  (ensure-existing-label label)
  (-> {:op :break}
      (inherit-context cenv :return? false)
      (cond-> label (assoc :label label))))

(defmethod parse-expr* 'return [cenv [_ value :as expr]]
  (let [context (context-of cenv)]
    (if (and (:expression context) (not (:tail context)))
      (error "cannot return from expression context")
      (let [{:keys [return-type] :as cenv'} (with-context cenv :expression)
            value' (case (count (rest expr))
                     0 (when-not (= return-type t/VOID)
                         (ensure-type cenv' return-type (parse-literal cenv' nil)))
                     1 (if (= return-type t/VOID)
                         (error "incompatible types: unexpected return value")
                         (->> (parse-expr cenv' value)
                              (ensure-type cenv' return-type)))
                     (error "return statement cannot take more than one arguments"))
            cenv' (cond-> cenv value' (with-context :expression))]
        (-> {:op :return :type (or (:type value') t/VOID)}
            (cond-> value' (assoc :value value'))
            (inherit-context cenv')
            (update :context conj :return))))))

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
      (err/error-on-incompatible-types t/INT (:type x))))

(defn- parse-array-creation [cenv type' [_ type & args :as expr]]
  (let [cenv' (with-context cenv :expression)]
    (if (vector? (first args))
      (let [elem-type (t/element-type type')
            elems' (map (fn [e]
                          (->> (if (vector? e) `(jise.core/new ~(t/type->tag elem-type) ~e) e)
                               (parse-expr cenv')
                               (ensure-type cenv' elem-type)))
                        (first args))]
        (-> {:op :new-array
             :type type'
             :dims [(parse-literal cenv' (count elems'))]
             :elements (vec elems')}
            (inherit-context cenv)))
      (let [depth (loop [t type' d 0]
                    (if (t/array-type? t)
                      (recur (t/element-type t) (inc d))
                      d))]
        (if (> depth (inc (count args)))
          (error "array dimension missing")
          (-> {:op :new-array
               :type type'
               :dims (mapv #(ensure-numeric-int cenv' (parse-expr cenv' %)) args)}
              (inherit-context cenv)))))))

(defn- args-for [cenv ctor-or-method args args' & {:keys [synthetic-params]}]
  (let [{:keys [conversions param-types]} ctor-or-method
        ncs (count conversions)
        varargs (when (not= (count param-types) ncs)
                  (let [vararg-type (peek param-types)
                        varargs-form `(jise.core/new ~(t/type->tag vararg-type)
                                                     ~(vec (drop ncs args)))]
                    (parse-expr cenv varargs-form)))
        cenv' (with-context cenv :expression)
        synthetic-params' (when (seq synthetic-params)
                            (parse-expr cenv' synthetic-params))]
    (as-> (mapv apply-conversions conversions args') args
      varargs (conj args varargs)
      synthetic-params' (into synthetic-params' args))))

(defmethod parse-expr* 'new [cenv [_ type & args :as expr]]
  (let [type' (resolve-type cenv type)
        abstract? (t/abstract-type? cenv type')]
    (when (or (t/primitive-type? type')
              (and (not (t/array-type? type')) abstract?))
      (error (str (err/stringify-type type')
                  (when abstract? " is abstract;")
                  " cannot be instantiated")))
    (if (t/array-type? type')
      (parse-array-creation cenv type' expr)
      (let [cenv' (with-context cenv :expression)
            args' (map (partial parse-expr cenv') args)
            arg-types (map :type args')
            ctors (try
                    (t/find-ctors cenv (:class-type cenv) type' arg-types)
                    (catch Exception e (err/handle-ctor-error type' arg-types e)))
            ctor (first ctors)
            synthetic-names (when (= type' (:class-type cenv'))
                              (:synthetic-fields cenv'))]
        (-> {:op :new
             :type type'
             :ctor (assoc ctor :class type')
             :args (args-for cenv' ctor args args'
                             :synthetic-params synthetic-names)}
            (inherit-context cenv))))))

(defn- setup-synthetic-fields [cenv fields]
  (->> `(jise.core/do
          ~@(for [field fields
                  :let [field' (with-meta field nil)]]
              `(jise.core/set! (. jise.core/this ~field) ~field)))
       (parse-expr (with-context cenv :statement))))

(defn- parse-ctor-invocation [cenv [op & args]]
  (let [super? (= (misc/fixup-ns op) 'super)
        cenv' (with-context cenv :expression)
        args' (map (partial parse-expr cenv') args)
        class (if super?
                (find-in-current-class cenv :parent)
                (:class-type cenv))
        arg-types (map :type args')
        ctors (try
                (t/find-ctors cenv (:class-type cenv) class arg-types)
                (catch Exception e (err/handle-ctor-error class arg-types e)))
        ctor (first ctors)
        synthetic-names (not-empty (:synthetic-fields cenv'))
        initializer (when super? (find-in-current-class cenv :initializer))
        node (as-> {:op :ctor-invocation
                    :ctor (assoc ctor :class class)
                    :args (args-for cenv' ctor args args'
                                    :synthetic-params
                                    (when-not super? synthetic-names))}
                 node
               (if (and super? synthetic-names)
                 {:op :do
                  :exprs [(setup-synthetic-fields cenv' synthetic-names) node]}
                 node))]
    (if initializer
      (-> {:op :do
           :exprs [(with-context node :statement)
                   (parse-expr cenv initializer)]}
          (inherit-context cenv :return? false))
      (inherit-context node cenv))))

(defmethod parse-expr* 'this [cenv expr]
  (parse-ctor-invocation cenv expr))

(defmethod parse-expr* 'super [cenv expr]
  (parse-ctor-invocation cenv expr))

(declare parse-alength)

(defn- parse-field-access [cenv callee callee-type fname]
  (if (and (t/array-type? callee-type) (= fname "length"))
    (if (nil? callee)
      (error "class expected")
      (parse-alength cenv callee))
    (if-let [{:keys [type used?] :as field} (get (:enclosing-env cenv) fname)]
      (do (reset! used? true)
          (-> {:op :field-access
               :type type
               :field (assoc field :class (:class-type cenv) :name fname)
               :target callee}
              (inherit-context cenv)))
      (if-let [{:keys [access] :as field} (find-field cenv callee-type fname)]
        (if (and (:final access) (contains? field :value))
          (parse-literal cenv (:value field))
          (-> {:op :field-access
               :type (:type field)
               :field (assoc field :name fname)}
              (inherit-context cenv)
              (cond-> callee (assoc :target callee))))
        (error (str "cannot find symbol\n"
                    "  symbol: variable " fname "\n"
                    "  location: class " (err/stringify-type callee-type)))))))

(defn- parse-method-invocation [cenv callee callee-type mname args]
  (let [cenv' (with-context cenv :expression)
        args' (map (partial parse-expr cenv') args)
        arg-types (map :type args')
        method (try
                 (let [ms (t/find-methods cenv (:class-type cenv) callee-type mname arg-types)]
                   (if (> (count ms) 1)
                     (throw (ex-info "ambiguous invocation"
                                     {:cause :ambiguous-invocation :alternatives ms}))
                     (first ms)))
                 (catch Exception e
                   (err/handle-method-error cenv callee-type mname arg-types e)))
        callee' (if (and (nil? callee)
                         (= callee-type (:class-type cenv))
                         (not (:static (:access method))))
                  ;; for method invocation omitting receiver such as (method args ...)
                  (parse-expr cenv' 'jise.core/this)
                  callee)]
    (when (and (nil? callee') (not (:static (:access method))))
      (error "class expected"))
    (-> {:op :method-invocation
         :type (:return-type method)
         :method (assoc method :name mname)
         :args (args-for cenv' method args args')}
        (inherit-context cenv)
        (cond->
            callee' (assoc :target callee')
            (= (:op callee') :super) (assoc :super? true)))))

(defmethod parse-expr* '. [cenv [_ callee property & maybe-args :as expr]]
  (if (and (seq? property) (nil? maybe-args))
    (parse-expr cenv (with-meta `(. ~callee ~@property) (meta expr)))
    (let [pname (name property)]
      (if (or (= pname "class") (= pname "-class"))
        (if-let [t (t/tag->type cenv callee :throws-on-failure? false)]
          (parse-literal cenv t)
          (error (str "cannot find symbol\n  symbol: class " callee)))
        (let [cenv' (with-context cenv :expression)
              callee' (if (symbol? callee)
                        (parse-symbol cenv' callee :throws-on-failure? false)
                        (when (not (t/tag->type cenv' callee :throws-on-failure? false))
                          (parse-expr cenv' callee)))
              callee-type (or (:type callee') (resolve-type cenv callee))]
          (cond (t/primitive-type? callee-type)
                (error (str (err/stringify-type callee-type) " cannot be dereferenced"))

                (str/starts-with? pname "-")
                (if (empty? maybe-args)
                  (parse-field-access cenv callee' callee-type (subs pname 1))
                  (error "field access cannot take arguments"))

                :else
                (try
                  (parse-method-invocation cenv callee' callee-type pname maybe-args)
                  (catch Exception e
                    (if (empty? maybe-args)
                      (try
                        (parse-field-access cenv callee' callee-type pname)
                        (catch Exception _ (throw e)))
                      (throw e))))))))))

(defn- fold-aget [[_ arr index & indices :as expr]]
  (if (empty? indices)
    expr
    (recur
     (with-meta
       `(jise.core/aget (jise.core/aget ~arr ~index) ~@indices)
       (meta expr)))))

(defn- ensure-array-type [{:keys [type] :as x}]
  (when-not (t/array-type? type)
    (error (format "array required, but %s found" (err/stringify-type type))))
  x)

(defn- parse-alength [cenv arr]
  (-> {:op :array-length
       :type t/INT
       :array arr}
      (inherit-context cenv)))

(defmethod parse-expr* 'alength [cenv [_ arr :as expr]]
  (ensure-sufficient-arguments 1 expr)
  (let [arr' (-> (parse-expr (with-context cenv :expression) arr)
                 ensure-array-type)]
    (parse-alength cenv arr')))

(defmethod parse-expr* 'aget [cenv [_ arr index & indices :as expr]]
  (ensure-sufficient-arguments 2 expr :varargs? true)
  (if indices
    (parse-expr cenv (fold-aget expr))
    (let [cenv' (with-context cenv :expression)
          arr (ensure-array-type (parse-expr cenv' arr))
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
                 `(jise.core/aset (jise.core/aget ~arr ~@(butlast indices))
                                  ~(last indices)
                                  ~(last more))
                 (meta expr))]
      (parse-expr cenv form))
    (let [cenv' (with-context cenv :expression)
          arr (ensure-array-type (parse-expr cenv' arr))
          elem-type (t/element-type (:type arr))
          index' (ensure-numeric-int cenv' (parse-expr cenv' index))
          expr' (ensure-type cenv' elem-type (parse-expr cenv' (first more)))]
      (-> {:op :array-update
           :type elem-type
           :array arr
           :index index'
           :expr expr'}
          (inherit-context cenv)))))

(defmethod parse-expr* 'defclass [_ _]
  (err/error-on-reserved-word defclass))

(defmethod parse-expr* 'definterface [_ _]
  (err/error-on-reserved-word definterface))

(defmethod parse-expr* 'defenum [_ _]
  (err/error-on-reserved-word defenum))

(defmethod parse-expr* 'defannotation [_ _]
  (err/error-on-reserved-word defannotation))

(defmethod parse-expr* 'class [_ _]
  (err/error-on-reserved-word class))

(defmethod parse-expr* 'fn [_ _]
  (err/error-on-reserved-word fn))

(defmethod parse-expr* 'fn* [_ _]
  (err/error-on-reserved-word fn*))
