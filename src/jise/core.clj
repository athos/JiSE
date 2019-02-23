(ns jise.core
  (:require [jise.emit :as emit]
            [jise.parse :as parse])
  (:import [clojure.lang Compiler DynamicClassLoader]))

(defmacro defclass [cname & body]
  (let [cname' (str cname)
        bytecode (emit/emit-class (parse/parse-class &form))]
    (when *compile-files*
      (Compiler/writeClassFile cname' bytecode))
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
    ^:public ^int
    (defm m [^int x] 1))

 )
