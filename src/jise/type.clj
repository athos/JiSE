(ns jise.type
  (:require [clojure.string :as str])
  (:import [clojure.asm Opcodes Type]))

(set! *warn-on-reflection* true)

(def BOOLEAN Type/BOOLEAN_TYPE)
(def BYTE Type/BYTE_TYPE)
(def CHAR Type/CHAR_TYPE)
(def SHORT Type/SHORT_TYPE)
(def INT Type/INT_TYPE)
(def LONG Type/LONG_TYPE)
(def FLOAT Type/FLOAT_TYPE)
(def DOUBLE Type/DOUBLE_TYPE)
(def VOID Type/VOID_TYPE)
(def OBJECT (Type/getType Object))
(def STRING (Type/getType String))

(def primitive->type
  {'boolean BOOLEAN
   'byte BYTE
   'char CHAR
   'short SHORT
   'int INT
   'long LONG
   'float FLOAT
   'double DOUBLE
   'void VOID})

(def primitive-type? (set (vals primitive->type)))

(def ^:const primitive-array-types
  '{ints [int]
    shorts [short]
    longs [long]
    floats [float]
    doubles [double]
    chars [char]
    bytes [byte]
    booleans [boolean]})

(defn array-type? [^Type t]
  (= (.getSort t) Type/ARRAY))

(defn ^Type element-type [^Type t]
  (Type/getType (str/replace (.getDescriptor t) #"^\[" "")))

(defn ^Type array-type [^Type t]
  (Type/getType (str \[ (.getDescriptor t))))

(declare tag->type)

(defn tag->array-type [cenv tag]
  (let [elem-type (first tag)
        t (tag->type cenv elem-type :default ::not-found)]
    (when-not (= t ::not-found)
      (array-type t))))

(defn find-in-cenv [cenv tag]
  (if-let [alias (get (:aliases cenv) tag)]
    (recur cenv alias)
    (when (contains? (:classes cenv) tag)
      (Type/getType (str \L (str/replace (str tag) \. \/) \;)))))

(defn ^Type tag->type [cenv tag & {:keys [default] :or {default OBJECT}}]
  (or (cond (symbol? tag) (or (primitive->type tag)
                              (some-> (get primitive-array-types tag)
                                      (#(tag->type cenv % :default default)))
                              (find-in-cenv cenv tag)
                              (when-let [c (resolve tag)]
                                (when (class? c)
                                  (Type/getType ^Class c))))
            (class? tag) (Type/getType ^Class tag)
            (vector? tag) (tag->array-type cenv tag)
            :else nil)
      default))

(defn ^Class type->class [^Type t]
  (Class/forName (.getClassName t)))

(defn type->symbol [^Type t]
  (symbol (.getClassName t)))

(defn ^Type object-type [obj]
  (cond (boolean? obj) BOOLEAN
        (char? obj) CHAR
        (int? obj) INT
        (float? obj) FLOAT
        (string? obj) STRING
        :else nil))

(defn ^Type wider-type [t1 t2]
  (let [ts (hash-set t1 t2)]
    (or (ts DOUBLE)
        (ts FLOAT)
        (ts LONG)
        INT)))

(defn type-category [t]
  (if (#{LONG DOUBLE} t) 2 1))

(defn find-field [cenv ^Type class name]
  (let [class-name (type->symbol class)]
    ;; TODO: needs to search class hierarchy as well
    (if-let [f (get-in cenv [:classes class-name :fields name])]
      {:class class :type (:type f)}
      (let [f (.getField (type->class class) name)]
        {:class (tag->type cenv (.getDeclaringClass f))
         :type (tag->type cenv (.getType f))}))))
