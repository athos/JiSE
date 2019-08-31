(ns jise.macroexpand
  (:refer-clojure :exclude [macroexpand])
  (:require [jise.type :as t]))

(defn macroexpand [cenv form]
  (let [expanded (macroexpand-1 form)]
    (if (identical? expanded form)
      (if-let [[op & args] (and (seq? form)
                                (symbol? (first form))
                                (some->> (namespace (first form)) symbol (t/find-in-cenv cenv))
                                form)]
        (with-meta `(. ~(symbol (namespace op)) ~(symbol (name op)) ~@args) (meta form))
        form)
      (recur cenv
             (cond-> expanded
               (instance? clojure.lang.IObj expanded)
               (vary-meta merge (meta form)))))))
