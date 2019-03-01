(ns jise.fn
  (:refer-clojure :exclude [defn fn])
  (:require [clojure.core :as c]
            [jise.core :as jise]))

(c/defn- emit-fn-class [fname args body]
  `^:public
  (jise/class ~fname [clojure.lang.IFn]
    ^:public ^Object
    (jise/defm ~'invoke ~args ~@body)))

(defmacro fn [args & body]
  (emit-fn-class (gensym 'f) args body))

(defmacro defn [name args & body]
  `(def ~name (fn ~args ~@body)))
