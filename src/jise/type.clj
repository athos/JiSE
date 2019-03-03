(ns jise.type
  (:require [clojure.string :as str])
  (:import [clojure.asm Opcodes Type]))

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

(defn maybe-array-type [tag]
  (and (vector? tag)
       (let [elem-type (first tag)
             t (tag->type elem-type :default ::not-found)]
         (when-not (= t ::not-found)
           (array-type t)))))

(defn ^Type tag->type [tag & {:keys [default]}]
  (or (primitive->type tag)
      (some-> (get primitive-array-types tag)
              (tag->type :default default))
      (and (class? tag) (Type/getType ^Class tag))
      (when-let [c (and (symbol? tag) (resolve tag))]
        (when (class? c)
          (Type/getType c)))
      (maybe-array-type tag)
      default
      OBJECT))

(defn ^Class type-class [^Type t]
  (Class/forName (.getClassName t)))

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
