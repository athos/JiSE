(ns jise.utils
  (:refer-clojure :exclude [defn fn let])
  (:require [clojure.core :as c]
            [jise.core :as jise]))

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

(defmacro def-type->primitive-interface []
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
  (jise/class ~fname [clojure.lang.IFn]
    ^:public ^Object
    (jise/defm ~'invoke ~(vec args')
      ~@(emit-fn-body args args' body))))

(defn- emit-prim-fn-class [fname prim return-type args args' body]
  (c/let [args'' (fixup-type-hints args :allow-primitive? false)]
    `^:public
    (jise/class ~fname [clojure.lang.IFn ~prim]
      ~(with-meta
         `(jise/defm ~'invokePrim ~(vec args')
            ~@(emit-fn-body args args' body))
         {:public true :tag return-type})
      ^:public ^Object
      (jise/defm ~'invoke ~(vec args'')
        ~@(emit-fn-body args' args'' body)))))

(defn- emit-fn-class [fname args body]
  (c/let [args' (fixup-type-hints args)
          return-type (primitive-type (:tag (meta args)))]
    (if-let [prim (primitive-interface args' return-type)]
      (emit-prim-fn-class fname prim return-type args args' body)
      (emit-ordinary-fn-class fname args args' body))))

(defmacro fn [args & body]
  (emit-fn-class (gensym 'f) args body))

(defmacro defn [name args & body]
  (c/let [return-type (primitive-type (:tag (meta args)))
          args' (cond-> (fixup-type-hints args)
                  return-type
                  (with-meta {:tag return-type}))]
    `(def ~(with-meta name {:arglists `'(~args')})
       (fn ~args ~@body))))

(defmacro let [bindings & body]
  (c/let [bindings' (partition 2 bindings)
          params (map first bindings')
          args (map second bindings')]
    `((fn ~(vec params) ~@body) ~@args)))

(defmacro do [& body]
  `((fn [] ~@body)))
