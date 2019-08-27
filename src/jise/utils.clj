(ns jise.utils
  (:refer-clojure :exclude [defn fn deftype])
  (:require [clojure.core :as c]
            [jise.core :as jise])
  (:import [clojure.lang Compiler$LocalBinding]))

(defn- primitive-type [t]
  (case t
    (float double) 'double
    (byte short int long) 'long
    'Object))

(defn- fixup-type-hint [allow-primitive? arg]
  (let [{:keys [tag] :or {tag 'Object}} (meta arg)
        tag' (or (when allow-primitive?
                   (primitive-type tag))
                 'Object)]
    (cond-> arg
      (not= tag tag')
      (vary-meta assoc :tag tag'))))

(defn- fixup-type-hints [args & {:keys [allow-primitive?] :or {allow-primitive? true}}]
  (mapv (partial fixup-type-hint (and allow-primitive? (< (count args) 5))) args))

(defmacro ^:private def-type->primitive-interface []
  `(def ~'type->primitive-interface
     ~(letfn [(rec [n]
                (if (= n 0)
                  [[]]
                  (let [xs (rec (dec n))]
                    (for [x xs
                          c '[L D O]]
                      (conj x c)))))]
        (->> (for [cs (mapcat rec (range 1 6))
                   :when (not (every? '#{O} cs))]
               [`'~cs (symbol (apply str 'clojure.lang.IFn$ cs))])
             (into {})))))

(def-type->primitive-interface)

(defn- type-char [t]
  (case t
    long 'L
    double 'D
    'O))

(defn- primitive-interface [args return-type]
  (let [hints (mapv (c/fn [arg]
                      (-> (:tag (meta arg))
                          #{'long 'double}
                          type-char))
                    args)]
    (type->primitive-interface (conj hints (type-char return-type)))))

(defn- emit-fn-body [args args' body]
  (if-let [diffs (->> (map vector args args')
                      (remove (c/fn [[s s']] (identical? s s')))
                      seq)]
    `((let ~(into [] (mapcat (c/fn [[s s']] [s (with-meta s' nil)])) diffs)
        ~@body))
    body))

(defn- emit-fn-methods [[params & body]]
  (let [params' (fixup-type-hints params)
        return-type (primitive-type (:tag (meta params)))]
    (if-let [prim (primitive-interface params' return-type)]
      (let [params'' (fixup-type-hints params :allow-primitive? false)]
        [(with-meta
           `(jise/defm ~'invokePrim ~(vec params')
              ~@(emit-fn-body params params' body))
           {:public true :tag return-type})
         `^:public ^Object
         (jise/defm ~'invoke ~(vec params'')
           (.invokePrim (jise/cast ~prim jise/this) ~@params'))])
      [`^:public ^Object
       (jise/defm ~'invoke ~(vec params')
         ~@(emit-fn-body params params' body))])))

(defn- emit-fn-class [fname sigs]
  (let [{:keys [prims]}
        (reduce (c/fn [m [params]]
                  (let [arity (count params)
                          params' (fixup-type-hints params)
                          return-type (primitive-type (:tag (meta params)))]
                    (when ((:arities m) arity)
                      (throw (ex-info "Can't have 2 overloads with same arity" {})))
                    (as-> (update m :arities conj arity) m
                      (if-let [prim (primitive-interface params' return-type)]
                        (update m :prims conj prim)
                        m))))
                {:prims [] :arities #{}}
                sigs)]
    `^:public
    (jise/class ~fname [clojure.lang.AFunction clojure.lang.IFn ~@prims]
      ~@(mapcat emit-fn-methods sigs))))

(defmacro fn [& sigs]
  (let [[fname sigs] (if (symbol? (first sigs))
                       [(first sigs) (next sigs)]
                       [(gensym 'f) sigs])
        sigs (if (vector? (first sigs))
               (list sigs)
               sigs)]
    (emit-fn-class fname sigs)))

(defmacro defn [name & fdecl]
  (let [[fdecl m] (if (string? (first fdecl))
                    [(next fdecl) {:doc (first fdecl)}]
                    [fdecl {}])
        [fdecl m] (if (map? fdecl)
                    [(next fdecl) (merge m (first fdecl))]
                    [fdecl m])
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        fdecl' (for [[params & body] fdecl
                     :let [params' (fixup-type-hints params)
                           meta (meta params)]]
                 `(~(with-meta (vec params')
                      (cond-> meta (:tag meta) (update :tag primitive-type)))
                   ~@body))
        m (cond->> (merge {:arglists `'~(#'c/sigs fdecl')} m)
            (meta name)
            (merge (meta name)))
        name-with-meta (with-meta name m)]
    `(do
       ;; the following `declare` is necessary to allow self reference within fn definition
       (declare ~name-with-meta)
       (def ~name-with-meta
         (fn ~name ~@fdecl)))))

(defmacro do [& body]
  `((fn [] ~@body)))

(defn- parse-opts+specs [opts+specs]
  (loop [opts+specs opts+specs
         interfaces []
         methods []]
    (if (empty? opts+specs)
      [interfaces methods]
      (let [[x & more] opts+specs]
        (if (symbol? x)
          (recur more (conj interfaces x) methods)
          (recur more interfaces (conj methods x)))))))

(defn- visibility-of [x]
  (let [m (meta x)]
    (cond (:public m) :public
          (:private m) :private
          :else :public)))

(defmacro deftype [name fields & opts+specs]
  (let [[interfaces methods] (parse-opts+specs opts+specs)
        ctor (with-meta
               `(jise/defm ~(with-meta name nil) ~fields
                  ~@(for [field fields
                          :let [field' (with-meta field nil)]]
                      `(jise/set! (jise/. jise/this ~field') ~field')))
               {:public true})
        fields' (for [field fields
                      :let [visibility (visibility-of field)]]
                  `(jise/def ~(with-meta field
                                (merge {visibility true}
                                       (dissoc (meta field) :public :private)))))
        methods' (for [[mname args & body :as method] methods
                       :let [visibility (visibility-of method)]]
                   (with-meta
                     `(jise/defm ~mname ~(vec (rest args))
                        (jise/let [~(first args) jise/this]
                          ~@body))
                     (merge {visibility true}
                            (dissoc (meta method) :public :private))))]
    `(do
       ^:public
       (jise/defclass ~name ~interfaces ~@fields' ~ctor ~@methods')
       (defn ~(symbol (str "->" name))
         ~(str "Positional factory function for class " name)
         ~(vec fields)
         (new ~name ~@(map #(with-meta % nil) fields))))))
