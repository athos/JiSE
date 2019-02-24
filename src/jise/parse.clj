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

(defn parse-modifiers [{:keys [tag] :as modifiers}]
  {:type (tag->type tag)
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

(defn parse-binary-op [cenv [_ x y] op]
  (let [lhs (parse-expr cenv x)
        rhs (parse-expr cenv y)
        t (wider-type (:type lhs) (:type rhs))]
    {:op op :type t
     :lhs (apply-conversion lhs t)
     :rhs (apply-conversion rhs t)}))

(defn parse-expr [cenv expr]
  (cond (seq? expr)
        (case (first expr)
          + (parse-binary-op cenv expr :add)
          - (parse-binary-op cenv expr :sub)
          * (parse-binary-op cenv expr :mul)
          / (parse-binary-op cenv expr :div)
          (assert false "not supported yet"))

        (nil? expr)
        {:op :null}

        (symbol? expr)
        (if-let [{:keys [index type]} ((:lenv cenv) (name expr))]
          {:op :local :index index :type type}
          (throw (ex-info (str "unknown variable found: " expr) {:variable expr})))

        :else
        (if-let [t (object-type expr)]
          {:op :literal :value expr
           :type (case t
                   (byte short int long) (:expected-type cenv 'int)
                   (float double) (:expected-type cenv 'double)
                   t)})))

(defn  parse-exprs [cenv body]
  (let [cenv' (dissoc cenv :expected-type)
        last (peek body)]
    (-> (mapv parse-expr (repeat cenv') (pop body))
        (conj (parse-expr cenv last)))))

(defn parse-method-arg [arg]
  (let [{:keys [access type]} (parse-modifiers (meta arg))]
    {:name (str arg)
     :type type
     :access (access-flags (meta arg))}))

(defn parse-method [cenv [_ mname args & body :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access type]} (parse-modifiers modifiers)
        args' (mapv parse-method-arg args)
        lenv (into {"this" {:index 0}}
                   (map-indexed (fn [i {:keys [name type]}]
                                  [name {:index (inc i) :type type}]))
                   args')
        cenv' (-> cenv
                  (assoc :expected-type type)
                  (assoc :lenv lenv))]
    {:name (str mname)
     :return-type type
     :args args'
     :access access
     :body (parse-exprs cenv' body)}))

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
