(ns jise.type
  (:require [clojure.string :as str])
  (:import [clojure.asm Opcodes Type]
           [java.lang.reflect Constructor Field Method Modifier]))

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

(def BOOLEAN_CLASS (Type/getType Boolean))
(def BYTE_CLASS (Type/getType Byte))
(def CHARACTER_CLASS (Type/getType Character))
(def SHORT_CLASS (Type/getType Short))
(def INTEGER_CLASS (Type/getType Integer))
(def LONG_CLASS (Type/getType Long))
(def FLOAT_CLASS (Type/getType Float))
(def DOUBLE_CLASS (Type/getType Double))

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

(def primitive-type?
  (comp boolean (set (vals primitive->type))))

(def integral-type? #{BYTE CHAR SHORT INT LONG})
(def numeric-type? (conj integral-type? FLOAT DOUBLE))

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
  (let [elem-type (first tag)]
    (when-let [t (tag->type cenv elem-type)]
      (array-type t))))

(defn find-in-cenv [cenv tag]
  (if-let [alias (get (:aliases cenv) tag)]
    (recur cenv alias)
    (when (contains? (:classes cenv) tag)
      (Type/getType (str \L (str/replace (str tag) \. \/) \;)))))

(defn ^Type tag->type [cenv tag & {:keys [allow-vararg-param-type?]}]
  (cond (symbol? tag) (or (primitive->type tag)
                          (some->> (get primitive-array-types tag) (tag->type cenv))
                          (find-in-cenv cenv tag)
                          (when-let [c (resolve tag)]
                            (when (class? c)
                              (Type/getType ^Class c)))
                          (when-let [[_ name] (re-matches #"(.+)\.\.\.$" (name tag))]
                            (if allow-vararg-param-type?
                              (tag->array-type cenv [(symbol name)])
                              (throw (ex-info "vararg param type not allowed here" {})))))
        (class? tag) (Type/getType ^Class tag)
        (vector? tag) (tag->array-type cenv tag)
        (string? tag) (Type/getType ^String tag)
        :else nil))

(def primitive-iname->class
  {"Z" Boolean/TYPE
   "B" Byte/TYPE
   "C" Character/TYPE
   "S" Short/TYPE
   "I" Integer/TYPE
   "J" Long/TYPE
   "F" Float/TYPE
   "D" Double/TYPE})

(defn ^Class type->class [^Type t]
  (let [iname (.getInternalName t)]
    (try
      (if (str/starts-with? iname "[")
        (Class/forName (str/replace iname #"/" "."))
        (or (primitive-iname->class iname)
            (resolve (symbol (.getClassName t)))))
      (catch ClassNotFoundException _))))

(defn type->symbol [^Type t]
  (symbol (.getClassName t)))

(def primitive-type->symbol
  {BOOLEAN 'boolean
   BYTE 'byte
   CHAR 'char
   SHORT 'short
   INT 'int
   LONG 'long
   FLOAT 'float
   DOUBLE 'double})

(defn type->tag [^Type t]
  (if (array-type? t)
    [(type->tag (element-type t))]
    (or (primitive-type->symbol t)
        (symbol (.getClassName t)))))

(def wider-primitive-types
  {BYTE #{SHORT INT LONG FLOAT}
   SHORT #{INT LONG FLOAT DOUBLE}
   CHAR #{INT LONG FLOAT DOUBLE}
   INT #{LONG FLOAT DOUBLE}
   LONG #{FLOAT DOUBLE}
   FLOAT #{DOUBLE}})

(def narrower-primitive-types
  {SHORT #{BYTE CHAR}
   CHAR #{BYTE SHORT}
   INT #{BYTE SHORT CHAR}
   LONG #{BYTE SHORT CHAR INT}
   FLOAT #{BYTE SHORT CHAR INT LONG}
   DOUBLE #{BYTE SHORT CHAR INT LONG FLOAT}})

(defn proper-primitive-super? [t1 t2]
  (get-in narrower-primitive-types [t1 t2]))

(def CLONEABLE (Type/getType Cloneable))
(def SERIARIZABLE (Type/getType java.io.Serializable))

(defn proper-reference-super? [cenv t1 t2]
  (or (= t1 OBJECT)
      (= t2 nil)
      (if (array-type? t2)
        (or (#{OBJECT CLONEABLE SERIARIZABLE} t1)
            (let [et (element-type t2)]
              (and (not (primitive-type? et))
                   (array-type? t1)
                   (proper-reference-super? cenv (element-type t1) et))))
        (loop [t t2]
          (if-let [{:keys [parent interfaces]} (get-in cenv [:classes (type->symbol t)])]
            (cond (or (= parent t1) (contains? interfaces t1)) true
                  (= parent OBJECT) false
                  :else (recur parent))
            (when-let [c (type->class t)]
              (when-let [c1 (type->class t1)]
                (contains? (supers c) c1))))))))

(defn super? [cenv t1 t2]
  (or (= t1 t2)
      (case [(primitive-type? t1) (primitive-type? t2)]
        [true  true ] (proper-primitive-super? t1 t2)
        [false false] (proper-reference-super? cenv t1 t2)
        false)))

(defn ^Type object-type [obj]
  (cond (boolean? obj) BOOLEAN
        (char? obj) CHAR
        (int? obj) INT
        (float? obj) FLOAT
        (string? obj) STRING
        :else nil))

(defn type-category ^long [t]
  (if (#{LONG DOUBLE} t) 2 1))

(defn modifiers->access-flags [ms]
  (cond-> #{}
    (Modifier/isAbstract ms) (conj :abstract)
    (Modifier/isFinal ms) (conj :final)
    (Modifier/isPrivate ms) (conj :private)
    (Modifier/isProtected ms) (conj :protected)
    (Modifier/isPublic ms) (conj :public)
    (Modifier/isStatic ms) (conj :static)))

(defn widening-primitive-conversion [from to]
  (when (get-in wider-primitive-types [from to])
    {:conversion :widening-primitive :from from :to to}))

(defn narrowing-primitive-conversion [from to]
  (when (get-in narrower-primitive-types [from to])
    {:conversion :narrowing-primitive :from from :to to}))

(def boxed-types
  {BOOLEAN BOOLEAN_CLASS
   BYTE BYTE_CLASS
   CHAR CHARACTER_CLASS
   SHORT SHORT_CLASS
   INT INTEGER_CLASS
   LONG LONG_CLASS
   FLOAT FLOAT_CLASS
   DOUBLE DOUBLE_CLASS})

(def unboxed-types
  (into {} (map (fn [[k v]] [v k])) boxed-types))

(defn boxing-conversion [t]
  (when-let [t' (boxed-types t)]
    {:conversion :boxing :from t :to t'}))

(defn unboxing-conversion [t]
  (when-let [t' (unboxed-types t)]
    {:conversion :unboxing :from t :to t'}))

(defn widening-reference-conversion [cenv from to]
  (when (and (not (primitive-type? from))
             (not (primitive-type? to))
             (proper-reference-super? cenv to from))
    {:conversion :widening-reference :from from :to to}))

(defn narrowing-reference-conversion [cenv from to]
  ;; FIXME: there are tons of rules to allow narrowing reference conversion
  (when (and (not (primitive-type? from))
             (not (primitive-type? to))
             (not (proper-reference-super? cenv to from)))
    {:conversion :narrowing-reference :from from :to to}))

(defn assignment-conversion [cenv from to]
  (if (= from to)
    []
    (case [(primitive-type? from) (primitive-type? to)]
      [true  true ] (when-let [c (widening-primitive-conversion from to)]
                      [c])
      [true  false] (let [box (boxing-conversion from)]
                      (or (and (= (:to box) to) [box])
                          (when-let [widen (widening-reference-conversion cenv (:to box) to)]
                            [box widen])))
      [false true ] (let [unbox (unboxing-conversion from)]
                      (or (and (= (:to unbox) to) [unbox])
                          (when-let [widen (widening-primitive-conversion (:to unbox) to)]
                            [unbox widen])))
      [false false] (when-let [c (widening-reference-conversion cenv from to)]
                      [c]))))

(defn casting-conversion [cenv from to]
  (if (= from to)
    []
    (case [(primitive-type? from) (primitive-type? to)]
      [true  true ] (when-let [c (or (widening-primitive-conversion from to)
                                     (narrowing-primitive-conversion from to))]
                      [c])
      [true  false] (let [box (boxing-conversion from)]
                      (or (and (= (:to box) to) [box])
                          (when-let [widen (widening-reference-conversion cenv (:to box) to)]
                            [box widen])))
      [false true ] (if (= from OBJECT)
                      (let [box (boxing-conversion to)]
                        [{:conversion :narrowing-reference :form from :to (:to box)}
                         {:conversion :unboxing :from (:to box) :to (:from box)}])
                      (let [unbox (unboxing-conversion from)]
                        (or (and (= (:to unbox) to) [unbox])
                            (when-let [widen (widening-primitive-conversion (:to unbox) to)]
                              [unbox widen]))))
      [false false] (when-let [c (or (widening-reference-conversion cenv from to)
                                     (narrowing-reference-conversion cenv from to))]
                      [c]))))

(defn unary-numeric-promotion [t]
  (condp contains? t
    #{BYTE_CLASS SHORT_CLASS CHARACTER_CLASS}
    (let [unbox (unboxing-conversion t)
          widen (widening-primitive-conversion (:to unbox) INT)]
      [unbox widen])

    #{INTEGER_CLASS LONG_CLASS FLOAT_CLASS DOUBLE_CLASS}
    [(unboxing-conversion t)]

    #{BYTE SHORT CHAR}
    [(widening-primitive-conversion t INT)]

    #{INT LONG}
    []

    nil))

(defn binary-numeric-promotion [t1 t2]
  (let [unbox1 (unboxing-conversion t1)
        unbox2 (unboxing-conversion t2)
        t1' (or (:to unbox1) t1)
        t2' (or (:to unbox2) t2)]
    (when (and (numeric-type? t1') (numeric-type? t2'))
      (let [widened (or (some (hash-set t1' t2') [DOUBLE FLOAT LONG]) INT)
            f (fn [t unbox]
                (let [widen (widening-primitive-conversion t widened)]
                  (cond-> []
                    unbox (conj unbox)
                    widen (conj widen))))]
        [(f t1' unbox1) (f t2' unbox2)]))))

(defn walk-class-hierarchy [^Class class f]
  (letfn [(walk [^Class c]
            (when c
              (concat (f c)
                      (mapcat walk (.getInterfaces c))
                      (walk (.getSuperclass c)))))]
   (walk class)))

(defn find-field [cenv ^Type class name]
  (let [class-name (type->symbol class)
        name' (munge name)]
    (letfn [(field->map [^Field f]
              {:class (tag->type cenv (.getDeclaringClass f))
               :type (tag->type cenv (.getType f))
               :access (modifiers->access-flags (.getModifiers f))})
            (walk [^Class c]
              (-> (walk-class-hierarchy c
                    (fn [^Class c]
                      (some->> (.getDeclaredFields c)
                               (filter #(= (.getName ^Field %) name'))
                               first
                               field->map
                               vector)))
                  first))]
      (if-let [entry (get-in cenv [:classes class-name])]
        (if-let [{:keys [type access]} (get-in entry [:fields name])]
          {:class class :type type :access access}
          ;; Here we assume all the superclasses and interfaces are defined outside of JiSE
          (let [{:keys [parent interfaces]} entry]
            (or (some walk (map type->class interfaces))
                (walk (type->class parent)))))
        (walk (type->class class))))))

(defn remove-overridden-methods [cenv methods]
  (->> methods
       (reduce (fn [ms {:keys [param-types] :as m}]
                 (if-let [m' (get ms param-types)]
                   (cond-> ms (super? cenv (:class m') (:class m)) (assoc param-types m))
                   (assoc ms param-types m)))
               {})
       vals))

(defn params-compatible? [nargs nparams varargs?]
  (or (= nargs nparams)
      (and varargs? (>= nargs (dec nparams)))))

(defn get-methods [cenv ^Type class name nargs]
  (let [class-name (type->symbol class)
        name' (munge name)]
    (letfn [(method->map [^Class c ^Method m]
              {:class (tag->type cenv (.getDeclaringClass m))
               :interface? (.isInterface c)
               :param-types (->> (.getParameterTypes m)
                                 (mapv (partial tag->type cenv)))
               :return-type (tag->type cenv (.getReturnType m))
               :access (cond-> (modifiers->access-flags (.getModifiers m))
                         (.isVarArgs m) (conj :varargs))})
            (walk [^Class c]
              (walk-class-hierarchy c
                (fn [^Class c]
                  (keep (fn [^Method m]
                          (when (and (= (.getName m) name')
                                     (params-compatible? nargs (.getParameterCount m) (.isVarArgs m)))
                            (method->map c m)))
                        (.getDeclaredMethods c)))))]
      (->> (if-let [entry (get-in cenv [:classes class-name])]
             (concat (->> (get-in entry [:methods name])
                          (keep (fn [{:keys [param-types access] :as m}]
                                  (when (params-compatible? nargs (count param-types) (:varargs access))
                                    (assoc m :class class)))))
                     (mapcat (comp walk type->class) (:interfaces entry))
                     (walk (type->class (:parent entry))))
             (walk (type->class class)))
           (remove-overridden-methods cenv)))))

(defn convert-arg-types-with [f param-types arg-types]
  (->> (map vector arg-types param-types)
       (reduce (fn [acc [at pt]]
                 (if-let [cs (f at pt)]
                   (conj acc cs)
                   (reduced nil)))
               [])))

(defn strict-invocation-conversion [cenv arg-types method]
  (letfn [(f [from to]
            (if (= from to)
              []
              (when-let [c (or (widening-primitive-conversion from to)
                               (widening-reference-conversion cenv from to))]
                [c])))]
    (when-let [cs (convert-arg-types-with f (:param-types method) arg-types)]
      (assoc method :conversions cs))))

(defn loose-invocation-conversion [cenv arg-types method]
  (when-let [cs (convert-arg-types-with (partial assignment-conversion cenv)
                                        (:param-types method)
                                        arg-types)]
    (assoc method :conversions cs)))

(defn variable-arity-invocation-conversion [cenv arg-types {:keys [param-types] :as method}]
  (let [required-param-types (butlast param-types)
        vararg-type (last param-types)]
    (when-let [cs (convert-arg-types-with (partial assignment-conversion cenv)
                                          required-param-types
                                          arg-types)]
      (let [nargs (count arg-types)
            nparams (count param-types)]
        (or (when-let [cs' (if (< nargs nparams)
                             []
                             (when-let [cs (and (= nargs nparams)
                                                (assignment-conversion cenv (last arg-types) vararg-type))]
                               [cs]))]
              (assoc method :conversions (into cs cs')))
            (when (convert-arg-types-with (partial assignment-conversion cenv)
                                          (repeat (element-type vararg-type))
                                          (drop (dec nparams) arg-types))
              (assoc method :conversions cs)))))))

(defn maximally-specific-methods [cenv methods]
  (filter (fn [m1]
            (every? (fn [m2]
                      (->> (map vector (:param-types m1) (:param-types m2))
                           (every? (fn [[p1 p2]] (super? cenv p2 p1)))))
                    methods))
          methods))

(defn filter-methods [cenv arg-types methods]
  (let [{fixed-arity's false, variable-arity's true} (group-by #(boolean (:varargs (:access %))) methods)
        filter-with #(seq (keep (partial %1 cenv arg-types) %2))]
    (some->> (or (filter-with strict-invocation-conversion fixed-arity's)
                 (filter-with loose-invocation-conversion fixed-arity's)
                 (filter-with variable-arity-invocation-conversion variable-arity's))
             (maximally-specific-methods cenv))))

(defn find-methods [cenv ^Type class name arg-types]
  (->> (get-methods cenv class name (count arg-types))
       (filter-methods cenv arg-types)))

(defn find-ctor [cenv ^Type class arg-types]
  (let [class-name (type->symbol class)
        ctors (get-in cenv [:classes class-name :ctors])]
    (or (some->> (seq ctors)
                 (filter #(= (:param-types %) arg-types))
                 first)
        (let [target-class (type->class class)
              arg-classes (map type->class arg-types)]
          (when-first [^Constructor ctor (->> (.getDeclaredConstructors target-class)
                                              (filter #(= (seq (.getParameterTypes ^Constructor %))
                                                          arg-classes)))]
            {:param-types (->> (.getParameterTypes ctor)
                               (mapv (partial tag->type cenv)))
             :access (modifiers->access-flags (.getModifiers ctor))})))))
