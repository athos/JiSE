(ns jise.parse)

(def ^:const primitive-types
  '#{int short long float double char boolean void})

(defn tag->type [tag & {:keys [default]}]
  (or (get primitive-types tag)
      (when-let [c (and (symbol? tag) (resolve tag))]
        (when (class? c) c))
      default
      Object))

(defn modifiers-of [[_ name :as form]]
  (merge (meta form) (meta name)))

(defn access-flags [modifiers]
  (cond-> #{}
    (:static modifiers) (conj :static)
    (:public modifiers) (conj :public)
    (:protected modifiers) (conj :protected)
    (:private modifiers) (conj :private)
    (:final modifiers) (conj :final)))

(defn parse-modifiers [{:keys [tag] :as modifiers} & {:keys [default-type]}]
  {:type (tag->type tag :default default-type)
   :access (access-flags modifiers)})

(defn object-type [obj]
  (cond (boolean? obj) 'boolean
        (char? obj) 'char
        (int? obj) 'int
        (float? obj) 'float
        (string? obj) String
        :else nil))

(defn parse-field [[_ fname value :as field]]
  (let [modifiers (modifiers-of field)
        {:keys [access type]} (parse-modifiers modifiers)]
    (cond-> {:name (str fname)
             :type type
             :access access}
      (not (nil? value)) (assoc :value value))))

(defn wider-type [t1 t2]
  (let [ts (hash-set t1 t2)]
    (or (ts 'double)
        (ts 'float)
        (ts 'long)
        'int)))

(defn apply-conversion [{:keys [type] :as x} t]
  (cond->> x
    (not= t type)
    (array-map :op :conversion :type t :src)))

(declare parse-expr)

(defmulti parse-expr* (fn [cenv expr] (first expr)))
(defmethod parse-expr* :default [cenv expr]
  (let [v (resolve (first expr))]
    (if (some-> v meta :macro)
      (parse-expr cenv (macroexpand expr))
      (assert false "not supported yet"))))

(defn parse-expr [cenv expr]
  (cond (seq? expr)
        (parse-expr* cenv expr)

        (nil? expr)
        {:op :null}

        (symbol? expr)
        (if-let [{:keys [index type]} ((:lenv cenv) (name expr))]
          {:op :local :index index :type type}
          (throw (ex-info (str "unknown variable found: " expr) {:variable expr})))

        :else
        (if-let [t (object-type expr)]
          (merge {:op :literal}
                 (case t
                   (byte short int long) {:type 'int :value (int expr)}
                   (float double) {:type 'double :value (double expr)}
                   {:type t :value expr})))))

(defn  parse-exprs [cenv body]
  (let [cenv' (dissoc cenv :expected-type)
        last' (parse-expr cenv (last body))]
    {:op :do :type (:type last')
     :exprs (-> (mapv parse-expr (repeat cenv') (butlast body))
                (conj last'))}))

(defn type-category [t]
  (case t
    (long double) 2
    1))

(defn parse-binding [cenv lname init]
  (let [init' (some->> init (parse-expr cenv))
        {:keys [access type]} (parse-modifiers (meta lname) :default-type (:type init'))
        lname' (str lname)]
    (cond-> {:name lname'
             :type type
             :access access
             :index (:next-index cenv)}
      init' (assoc :init init'))))

(defn parse-bindings [cenv bindings]
  (loop [[lname init & bindings] bindings
         cenv cenv
         ret []]
    (if lname
      (let [b (parse-binding cenv lname init)
            cenv' (-> cenv
                      (assoc-in [:lenv (:name b)] b)
                      (update :next-index + (type-category (:type b))))]
        (recur bindings cenv'(conj ret b)))
      [cenv ret])))

(defn parse-method [cenv [_ mname args & body :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access type]} (parse-modifiers modifiers :default-type 'void)
        init-lenv (if (:static access) {} {"this" 0})
        init-index (count init-lenv)
        [cenv' args'] (parse-bindings (assoc cenv :lenv init-lenv :next-index init-index)
                                      (interleave args (repeat nil)))]
    {:name (str mname)
     :return-type type
     :args args'
     :access access
     :body (parse-exprs (assoc cenv' :expected-type type) body)}))

(defn parse-class-body [body]
  (loop [decls body
         ret {:fields []
              :methods []}]
    (if (empty? decls)
      ret
      (let [[decl & decls] decls]
        (if (seq? decl)
          (case (first decl)
            def (recur decls (update ret :fields conj decl))
            defm (recur decls (update ret :methods conj decl))
            do (recur (concat (rest decl) decls) ret)
            (if-let [v (resolve (first decl))]
              (if (:macro (meta v))
                (recur (cons (macroexpand decl) decls) ret)
                (let [msg (str "unknown type of declaration found: " (first decl))]
                  (throw (ex-info msg {:decl decl}))))
              (let [msg (str "unknown type of declaration found: " (first decl))]
                (throw (ex-info msg {:decl decl})))))
          (recur decls ret))))))

(defn parse-class [[_ cname & body :as class]]
  (let [modifiers (modifiers-of class)
        {:keys [fields methods]} (parse-class-body body)
        cenv {}]
    {:name (str cname)
     :access (access-flags modifiers)
     :fields (mapv parse-field fields)
     :methods (mapv (partial parse-method cenv) methods)}))

(defn parse-binary-op [cenv [_ x y] op]
  (let [lhs (parse-expr cenv x)
        rhs (parse-expr cenv y)
        t (wider-type (:type lhs) (:type rhs))]
    {:op op
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

(defmethod parse-expr* '- [cenv expr]
  (parse-arithmetic cenv expr :div))

(defn parse-comparison [cenv expr op]
  (assoc (parse-binary-op cenv expr op) :type 'boolean))

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

(defmethod parse-expr* 'set! [cenv [_ lname expr]]
  {:op :assignment
   :lhs (parse-expr cenv lname)
   :rhs (parse-expr cenv expr)})

(defmethod parse-expr* 'if [cenv [_ test then else]]
  (cond-> {:op :if
           :test (parse-expr cenv test)
           :then (parse-expr cenv then)}
    else (assoc :else (parse-expr cenv else))))

(defmethod parse-expr* 'while [cenv [_ cond & body]]
  {:op :while
   :cond (parse-expr cenv cond)
   :body (parse-exprs cenv body)})

(defmethod parse-expr* 'for [cenv [_ [lname init cond step] & body]]
  (let [[cenv' bindings'] (parse-bindings cenv [lname init])]
    {:op :let
     :bindings bindings'
     :body
     {:op :for
      :cond (parse-expr cenv' cond)
      :step (parse-expr cenv' step)
      :body (parse-exprs cenv' body)}}))

(defmethod parse-expr* 'continue [_ _]
  {:op :continue})

(defmethod parse-expr* 'break [_ _]
  {:op :break})
