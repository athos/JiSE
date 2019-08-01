(ns jise.utils
  (:refer-clojure :exclude [defn fn let deftype])
  (:require [clojure.core :as c]
            [jise.core :as jise])
  (:import [clojure.lang Compiler$LocalBinding]))

(defn- primitive-type [t]
  (case t
    (float double) 'double
    (byte short int long) 'long
    'Object))

(defn- fixup-type-hint [allow-primitive? arg]
  (c/let [{:keys [tag] :or {tag 'Object}} (meta arg)
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
                  (c/let [xs (rec (dec n))]
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
  (c/let [hints (mapv (c/fn [arg]
                        (-> (:tag (meta arg))
                            #{'long 'double}
                            type-char))
                      args)]
    (type->primitive-interface (conj hints (type-char return-type)))))

(defn- emit-fn-body [args args' body]
  (if-let [diffs (->> (map vector args args')
                      (remove (c/fn [[s s']] (identical? s s')))
                      seq)]
    `((let* ~(into [] (mapcat (c/fn [[s s']] [s (with-meta s' nil)])) diffs)
        ~@body))
    body))

(defn- emit-ordinary-fn-class [fname args args' body]
  `^:public
  (jise/class ~fname [clojure.lang.AFunction clojure.lang.IFn]
    ^:public ^Object
    (jise/defm ~'invoke ~(vec args')
      ~@(emit-fn-body args args' body))))

(defn- emit-prim-fn-class [fname prim return-type args args' body]
  (c/let [args'' (fixup-type-hints args :allow-primitive? false)]
    `^:public
    (jise/class ~fname [clojure.lang.AFunction clojure.lang.IFn ~prim]
      ~(with-meta
         `(jise/defm ~'invokePrim ~(vec args')
            ~@(emit-fn-body args args' body))
         {:public true :tag return-type})
      ^:public ^Object
      (jise/defm ~'invoke ~(vec args'')
        (.invokePrim ~'this ~@args')))))

(defn- emit-fn-class [fname args body]
  (c/let [args' (fixup-type-hints args)
          return-type (primitive-type (:tag (meta args)))]
    (if-let [prim (primitive-interface args' return-type)]
      (emit-prim-fn-class fname prim return-type args args' body)
      (emit-ordinary-fn-class fname args args' body))))

(defmacro fn [& body]
  (c/let [[fname args body] (if (symbol? (first body))
                              [(first body) (second body) (nnext body)]
                              [(gensym 'f) (first body) (next body)])]
   (emit-fn-class fname args body)))

(defmacro defn [name args & body]
  (c/let [return-type (primitive-type (:tag (meta args)))
          args' (cond-> (fixup-type-hints args)
                  return-type
                  (with-meta {:tag return-type}))]
    `(def ~(with-meta name {:arglists `'(~args')})
       (fn ~name ~args ~@body))))

(defmacro let-internal [names body]
  (c/let [types (map (c/fn [name]
                       (c/let [^Compiler$LocalBinding lb (get &env name)]
                         (when (.hasJavaClass lb)
                           (.getJavaClass lb))))
                     names)
          names' (map (c/fn [name type]
                        (cond-> name
                          (and type (nil? (:tag (meta name))))
                          (with-meta {:tag type})))
                      names types)]
    `((fn ~(vec names') ~@body) ~@names)))

(defmacro let [bindings & body]
  (c/let [bindings' (partition 2 bindings)
          names (map first bindings')
          inits (map second bindings')]
    `(c/let [~@(interleave names inits)]
       (let-internal ~names ~body))))

(defmacro do [& body]
  `((fn [] ~@body)))

(defn- parse-opts+specs [opts+specs]
  (loop [opts+specs opts+specs
         interfaces []
         methods []]
    (if (empty? opts+specs)
      [interfaces methods]
      (c/let [[x & more] opts+specs]
        (if (symbol? x)
          (recur more (conj interfaces x) methods)
          (recur more interfaces (conj methods x)))))))

(defmacro deftype [name fields & opts+specs]
  (c/let [[interfaces methods] (parse-opts+specs opts+specs)
          ctor (with-meta
                 `(jise/defm ~(with-meta name nil) ~fields
                    ~@(for [field fields
                            :let [field' (with-meta field nil)]]
                        `(jise/set! (jise/. jise/this ~field') ~field')))
                 {:public true})
          fields' (for [field fields
                        :let [m (meta field)
                              visibility (cond (:public m) :public
                                               (:private m) :private
                                               :else :private)]]
                    (with-meta
                      `(jise/def ~(vary-meta field dissoc :public :private))
                      {visibility true}))
          methods' (for [[mname args & body :as method] methods]
                     (with-meta
                       `(jise/defm ~mname ~(vec (rest args))
                          (jise/let [~(first args) jise/this]
                            ~@body))
                       (meta method)))]
    `^:public (jise/defclass ~name ~interfaces ~@fields' ~ctor ~@methods')))
