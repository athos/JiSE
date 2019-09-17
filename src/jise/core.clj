(ns jise.core
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as str]
            [jise.emit :as emit]
            [jise.parse :as parse]
            [jise.type :as type])
  (:import [clojure.lang Compiler Compiler$LocalBinding DynamicClassLoader]))

(defn- qualify-cname [cname]
  (let [cname' (name cname)]
    (-> (cond->> cname'
          (neg? (.indexOf cname' "."))
          (str (ns-name *ns*) \.))
        (.replace \- \_)
        symbol
        (with-meta (meta cname)))))

(defn- compile-to-bytecode [source form-meta enclosing-env qname body]
  (->> (with-meta `(defclass ~qname ~@body) form-meta)
       (parse/parse-class enclosing-env)
       (#(cond-> % source (assoc :source source)))
       emit/emit-class))

(defn- compile-class [form-meta enclosing-env cname body]
  (let [qname (with-meta (qualify-cname cname) (meta cname))
        qname' (str qname)
        bytecode (compile-to-bytecode *source-path* form-meta enclosing-env qname body)]
    (when *compile-files*
      (Compiler/writeClassFile (str/replace qname' \. \/) bytecode))
    (.defineClass ^DynamicClassLoader @Compiler/LOADER qname' bytecode nil)
    qname))

(defn- enclosing-env [&env]
  (reduce-kv (fn [m sym ^Compiler$LocalBinding lb]
               (assoc m (name sym)
                      {:type (if (.hasJavaClass lb)
                               (type/tag->type {} (.getJavaClass lb))
                               type/OBJECT)
                       :foreign? true
                       :used? (atom false)}))
             {}
             &env))

(defmacro defclass [cname & body]
  (let [qname (compile-class (meta &form) {} cname body)]
    `(do (import '~qname)
         ~qname)))

(defmacro class [maybe-name & body]
  (let [cname (if (symbol? maybe-name)
                maybe-name
                (gensym 'C))
        enclosing-env (enclosing-env &env)
        body (if (symbol? maybe-name) body (cons maybe-name body))
        qname (compile-class (meta &form) enclosing-env cname body)]
    `(new ~qname
          ~@(for [[name {:keys [used?]}] enclosing-env
                  :when @used?]
              (symbol name)))))

(comment

  (require '[clojure.java.io :as io])
  (import 'java.io.DataOutputStream)

  (defn gen [[_ cname & body :as class] filename]
    (let [qname (with-meta (qualify-cname cname) (meta cname))
          qname' (str qname)
          bytecode (compile-to-bytecode nil (meta class) {} qname body)]
      (with-open [out (DataOutputStream. (io/output-stream filename))]
        (.write out bytecode)
        (.flush out))))

 )
