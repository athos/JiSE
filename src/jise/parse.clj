(ns jise.parse
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.string :as str]))

(def ^:const primitive-types
  '#{int short long float double char byte boolean void})

(def ^:const primitive-array-types
  '{ints [int]
    shorts [short]
    longs [long]
    floats [float]
    doubles [double]
    chars [char]
    bytes [byte]
    booleans [boolean]})

(defn array-type? [t]
  (vector? t))

(defn element-type [t]
  (first t))

(declare tag->type)

(defn maybe-array-type [tag]
  (and (array-type? tag)
       (let [elem-type (element-type tag)
             t (tag->type elem-type :default ::not-found)]
         (when-not (= t ::not-found)
           [t]))))

(defn tag->type [tag & {:keys [default]}]
  (or (get primitive-types tag)
      (get primitive-array-types tag)
      (when-let [c (and (symbol? tag) (resolve tag))]
        (when (class? c) c))
      (maybe-array-type tag)
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

(defn macroexpand [form]
  (let [v (resolve (first form))]
    (if (some-> v meta :macro)
      (let [expanded (macroexpand-1 form)]
        (if (identical? expanded form)
          form
          (recur (cond-> expanded
                   (instance? clojure.lang.IObj expanded)
                   (vary-meta merge (meta form))))))
      form)))

(declare parse-expr)

(defmulti parse-expr* (fn [cenv expr] (first expr)))
(defmethod parse-expr* :default [cenv expr]
  (let [v (resolve (first expr))]
    (if (some-> v meta :macro)
      (parse-expr cenv (macroexpand expr))
      (assert false "not supported yet"))))

(defn with-context [x context]
  (assoc x :context context))

(defn inherit-context [x {:keys [context]}]
  (with-context x context))

(defn parse-expr [cenv expr]
  (cond (seq? expr)
        (let [expr' (parse-expr* cenv expr)]
          (if-let [label (:label (meta expr))]
            {:op :labeled :label label :target expr'}
            expr'))

        (nil? expr)
        (inherit-context {:op :null} cenv)

        (symbol? expr)
        (if-let [{:keys [index type]} ((:lenv cenv) (name expr))]
          (inherit-context {:op :local :index index :type type} cenv)
          (throw (ex-info (str "unknown variable found: " expr) {:variable expr})))

        :else
        (if-let [t (object-type expr)]
          (merge (inherit-context {:op :literal} cenv)
                 (case t
                   (byte short int long) {:type 'int :value (int expr)}
                   (float double) {:type 'double :value (double expr)}
                   {:type t :value expr})))))

(defn  parse-exprs [cenv body]
  (let [cenv' (with-context cenv :statement)
        last' (parse-expr cenv (last body))]
    {:op :do :type (:type last')
     :exprs (-> (mapv parse-expr (repeat cenv') (butlast body))
                (conj last'))}))

(defn type-category [t]
  (case t
    (long double) 2
    1))

(defn parse-binding [cenv lname init]
  (let [init' (some->> init (parse-expr (with-context cenv :expression)))
        {:keys [access type]} (parse-modifiers (meta lname) :default-type (:type init'))
        lname' (str lname)]
    (cond-> {:name lname'
             :type type
             :access access
             :index (:next-index cenv)}
      init' (assoc :init init'))))

(defn parse-bindings [cenv bindings]
  (loop [[lname init & bindings] bindings
         cenv' (with-context cenv :expression)
         ret []]
    (if lname
      (let [b (parse-binding cenv' lname init)
            cenv' (-> cenv'
                      (assoc-in [:lenv (:name b)] b)
                      (update :next-index + (type-category (:type b))))]
        (recur bindings cenv' (conj ret b)))
      [(inherit-context cenv' cenv) ret])))

(defn parse-method [cenv [_ mname args & body :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access type]} (parse-modifiers modifiers :default-type 'void)
        init-lenv (if (:static access) {} {"this" 0})
        init-index (count init-lenv)
        [cenv' args'] (parse-bindings (assoc cenv :lenv init-lenv :next-index init-index)
                                      (interleave args (repeat nil)))
        context (if (= type 'void) :statement :return)]
    {:name (str mname)
     :return-type type
     :args args'
     :access access
     :body (parse-exprs (with-context cenv' context) body)}))

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
    {:name (str/replace (str cname) \. \/)
     :access (access-flags modifiers)
     :fields (mapv parse-field fields)
     :methods (mapv (partial parse-method cenv) methods)}))

(defn parse-binary-op [cenv [_ x y] op]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' x)
        rhs (parse-expr cenv' y)
        t (wider-type (:type lhs) (:type rhs))]
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

(defmethod parse-expr* '- [cenv expr]
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

(defmethod parse-expr* 'set! [cenv [_ lname expr]]
  (let [cenv' (with-context cenv :expression)
        lhs (parse-expr cenv' lname)]
    {:op :assignment
     :type (:type lhs)
     :context (:context cenv)
     :lhs lhs
     :rhs (parse-expr cenv' expr)}))

(defmethod parse-expr* 'inc! [cenv [_ lname by]]
  (let [by (or by 1)
        {:keys [type] :as lname'} (parse-expr (with-context cenv :expression) lname)]
    (if (and (= (wider-type (:type lname') 'int) 'int)
             (pos-int? by))
      (inherit-context {:op :increment, :target lname', :type type, :by by} cenv)
      (parse-expr cenv `(set! ~lname (~'+ ~lname ~by))))))

(defmethod parse-expr* 'dec! [cenv [_ lname by]]
  (let [by (or by 1)
        {:keys [type] :as lname'} (parse-expr (with-context cenv :expression) lname)]
    (if (and (= (wider-type (:type lname') 'int) 'int)
             (pos-int? by))
      (inherit-context {:op :increment, :target lname', :type type, :by (- by)} cenv)
      (parse-expr cenv `(set! ~lname (~'- ~lname ~by))))))

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

(defmethod parse-expr* 'new [cenv [_ type arg]]
  (let [type' (tag->type type)]
    (if (array-type? type')
      (if (vector? arg)
        (let [arr (gensym)]
          (parse-expr cenv `(~'let [~arr (new ~type ~(count arg))]
                             ~@(for [[i init] (map-indexed vector arg)]
                                 `(~'aset ~arr ~i ~init))
                             ~arr)))
        {:op :new-array
         :context (:context cenv)
         :type type'
         :length (parse-expr (with-context cenv :expression) arg)})
      (throw (ex-info (str "Construction of type " type " not supported yet") {:type type})))))

(defmethod parse-expr* 'aget [cenv [_ arr index]]
  (let [cenv' (with-context cenv :expression)
        arr (parse-expr cenv' arr)]
    {:op :array-access
     :context (:context cenv)
     :type (element-type (:type arr))
     :array arr
     :index (parse-expr cenv' index)}))

(defmethod parse-expr* 'aset [cenv [_ arr index expr]]
  (let [cenv' (with-context cenv :expression)
        arr (parse-expr cenv' arr)]
    {:op :array-update
     :context (:context cenv)
     :type (element-type (:type arr))
     :array arr
     :index (parse-expr cenv' index)
     :expr (parse-expr cenv' expr)}))
