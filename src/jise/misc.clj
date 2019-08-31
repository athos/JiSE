(ns jise.misc)

(defn strip-jise-ns [sym]
  (if (= (namespace sym) "jise.core")
    (symbol (name sym))
    sym))

(defn resolve-ns [sym]
  (let [ns (namespace sym)]
    (if-let [orig (some->> ns symbol (get (ns-aliases *ns*)))]
      (symbol (str orig) (name sym))
      sym)))

(defn fixup-ns [sym]
  (strip-jise-ns (resolve-ns sym)))
