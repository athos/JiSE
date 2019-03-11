(ns jise.fn
  (:refer-clojure :exclude [defn fn])
  (:require [clojure.core :as c]
            [jise.core :as jise]))

(c/defn- fixup-type-hints [args]
  (map (c/fn [arg]
         (if (= (:tag (meta arg) 'Object) 'Object)
           arg
           (vary-meta arg dissoc :tag)))
       args))

(c/defn- emit-fn-class [fname args body]
  (let [args' (fixup-type-hints args)]
    `^:public
    (jise/class ~fname [clojure.lang.IFn]
      ^:public ^Object
      (jise/defm ~'invoke ~(vec args')
        ~@(if-let [diffs (->> (map vector args args')
                              (remove (c/fn [[s s']] (identical? s s')))
                              seq)]
            `((let* ~(into [] cat diffs)
                ~@body))
            body)))))

(defmacro fn [args & body]
  (emit-fn-class (gensym 'f) args body))

(defmacro defn [name args & body]
  `(def ~name (fn ~args ~@body)))
