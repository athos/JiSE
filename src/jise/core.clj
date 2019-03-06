(ns jise.core
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as str]
            [jise.emit :as emit]
            [jise.parse :as parse])
  (:import [clojure.lang Compiler DynamicClassLoader]))

(defn qualify-cname [cname]
  (let [cname' (name cname)]
    (-> (cond->> cname'
          (neg? (.indexOf cname' "."))
          (str (ns-name *ns*) \.))
        (.replace \- \_)
        symbol
        (with-meta (meta cname)))))

(defn compile-class [form-meta cname body]
  (let [qname (with-meta (qualify-cname cname) (meta cname))
        qname' (str qname)
        bytecode (-> `(defclass ~qname ~@body)
                     (with-meta form-meta)
                     parse/parse-class
                     emit/emit-class)]
    (when *compile-files*
      (Compiler/writeClassFile (str/replace qname' \. \/) bytecode))
    (.defineClass ^DynamicClassLoader @Compiler/LOADER qname' bytecode nil)
    qname))

(defmacro defclass [cname & body]
  (let [qname (compile-class (meta &form) cname body)]
    `(do (import '~qname)
         ~qname)))

(defmacro class [maybe-name & body]
  (let [cname (if (symbol? maybe-name)
                maybe-name
                `C#)]
    `(new ~(compile-class (meta &form) cname body))))

(comment

  (require '[clojure.java.io :as io])
  (import 'java.io.DataOutputStream)

  (defn gen [class filename]
    (let [bytecode (emit/emit-class (parse/parse-class class))]
      (with-open [out (DataOutputStream. (io/output-stream filename))]
        (.write out bytecode)
        (.flush out))))

  ^:public
  (defclass Qsort
    ^:public ^:static
    (defm qsort [^{:tag [int]} xs ^int left ^int right]
      (when (< left right)
        (let [p (aget xs (/ (+ left right) 2))
              l left
              r right]
          (while (<= l r)
            (while (< (aget xs l) p) (inc! l))
            (while (> (aget xs r) p) (dec! r))
            (when (<= l r)
              (let [tmp (aget xs l)]
                (aset xs l (aget xs r))
                (aset xs r tmp)
                (inc! l)
                (dec! r))))
          (Qsort/qsort xs left r)
          (Qsort/qsort xs l right)))))

 )
