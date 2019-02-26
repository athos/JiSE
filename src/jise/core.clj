(ns jise.core
  (:require [clojure.string :as str]
            [jise.emit :as emit]
            [jise.parse :as parse])
  (:import [clojure.lang Compiler DynamicClassLoader]))

(defn qualify-cname [cname]
  (let [cname' (name cname)]
    (-> (cond->> cname'
          (neg? (.indexOf cname' "."))
          (str (ns-name *ns*) \.))
        symbol
        (with-meta (meta cname)))))

(defmacro defclass [cname & body]
  (let [cname (qualify-cname cname)
        cname' (str cname)
        bytecode (-> `(defclass ~cname ~@body)
                     (with-meta (meta &form))
                     parse/parse-class
                     emit/emit-class)]
    (when *compile-files*
      (Compiler/writeClassFile (str/replace cname' \. \/) bytecode))
    (.defineClass ^DynamicClassLoader @Compiler/LOADER cname' bytecode nil)
    `(do (import '~cname)
         ~cname)))

(comment

  (require '[clojure.java.io :as io])
  (import 'java.io.DataOutputStream)

  (defn gen [class filename]
    (let [bytecode (emit/emit-class (parse/parse-class class))]
      (with-open [out (DataOutputStream. (io/output-stream filename))]
        (.write out bytecode)
        (.flush out))))

  ^:public
  (defclass C
    ^:public ^String
    (def x nil)
    ^:public ^boolean
    (defm isZero [^int x]
      (if (== x 0) true false)))

 )
