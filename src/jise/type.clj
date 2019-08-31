(ns jise.type
  (:require [clojure.string :as str]
            [jise.misc :as misc])
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
(def CLASS (Type/getType Class))
(def THROWABLE (Type/getType Throwable))

(def BOOLEAN_CLASS (Type/getType Boolean))
(def BYTE_CLASS (Type/getType Byte))
(def CHARACTER_CLASS (Type/getType Character))
(def SHORT_CLASS (Type/getType Short))
(def INTEGER_CLASS (Type/getType Integer))
(def LONG_CLASS (Type/getType Long))
(def FLOAT_CLASS (Type/getType Float))
(def DOUBLE_CLASS (Type/getType Double))

(def ^:private primitive->type
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

(defn boolean-type? [t]
  (or (= t BOOLEAN) (= t BOOLEAN_CLASS)))

(def ^:private ^:const primitive-array-types
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

(defn- tag->array-type [cenv tag & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
  (let [elem-type (first tag)]
    (when-let [t (tag->type cenv elem-type :throws-on-failure? throws-on-failure?)]
      (array-type t))))

(defn find-in-cenv [cenv tag]
  (if-let [alias (get (:aliases cenv) tag)]
    (recur cenv alias)
    (when (contains? (:classes cenv) tag)
      (Type/getType (str \L (str/replace (str tag) \. \/) \;)))))

(defn ^Type tag->type
  ([tag] (tag->type {} tag))
  ([cenv tag & {:keys [allow-vararg-param-type? throws-on-failure?] :or {throws-on-failure? true}}]
   (letfn [(fail []
             (when throws-on-failure?
               (throw (ex-info (str "cannot resolve type " (pr-str tag))
                               {:cause :unresolved-type :tag tag}))))]
     (cond (symbol? tag) (if (namespace tag)
                           (let [tag' (misc/fixup-ns tag)]
                             (if (namespace tag')
                               (fail)
                               (tag->type cenv tag'
                                          :allow-vararg-param-type? allow-vararg-param-type?
                                          :throws-on-failure? throws-on-failure?)))
                           (or (primitive->type tag)
                               (as-> (get primitive-array-types tag) t
                                 (tag->type cenv t
                                            :allow-vararg-param-type? allow-vararg-param-type?
                                            :throws-on-failure? throws-on-failure?))
                               (find-in-cenv cenv tag)
                               (when-let [c (resolve tag)]
                                 (when (class? c)
                                   (Type/getType ^Class c)))
                               (when-let [[_ name] (re-matches #"(.+)\.\.\.$" (name tag))]
                                 (if allow-vararg-param-type?
                                   (tag->array-type cenv [(symbol name)]
                                                    :throws-on-failure? throws-on-failure?)
                                   (throw (ex-info "vararg param type not allowed here"
                                                   {:cause :invalid-vararg-param-type}))))
                               (fail)))
           (class? tag) (Type/getType ^Class tag)
           (vector? tag) (tag->array-type cenv tag :throws-on-failure? throws-on-failure?)
           (nil? tag) nil
           :else (fail)))))

(def ^:private primitive-iname->class
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

(defn ^Type class->type [^Class class]
  (Type/getType class))

(defn type->symbol [^Type t]
  (symbol (.getClassName t)))

(def ^:private primitive-type->symbol
  {BOOLEAN 'boolean
   BYTE 'byte
   CHAR 'char
   SHORT 'short
   INT 'int
   LONG 'long
   FLOAT 'float
   DOUBLE 'double})

(defn type->tag [^Type t]
  (cond (nil? t) nil
        (array-type? t) [(type->tag (element-type t))]
        :else (or (primitive-type->symbol t)
                  (symbol (.getClassName t)))))

(defn modifiers->access-flags [ms]
  (cond-> #{}
    (Modifier/isAbstract ms) (conj :abstract)
    (Modifier/isFinal ms) (conj :final)
    (Modifier/isPrivate ms) (conj :private)
    (Modifier/isProtected ms) (conj :protected)
    (Modifier/isPublic ms) (conj :public)
    (Modifier/isStatic ms) (conj :static)
    (Modifier/isTransient ms) (conj :transient)
    (Modifier/isVolatile ms) (conj :volatile)
    (->> (bit-or Modifier/PUBLIC Modifier/PROTECTED Modifier/PRIVATE)
         (bit-and ms)
         (= 0))
    (conj :package)))

(defn- modifiers-of [cenv t]
  (when-not (primitive-type? t)
    (let [tag (type->tag t)]
      (or (some-> (get-in cenv [:classes tag]) :access)
          (when-let [^Class class (type->class t)]
            (modifiers->access-flags (.getModifiers class)))))))

(defn final-type? [cenv t]
  (:final (modifiers-of cenv t)))

(defn abstract-type? [cenv t]
  (:abstract (modifiers-of cenv t)))

(def ^:private wider-primitive-types
  {BYTE #{SHORT INT LONG FLOAT DOUBLE}
   SHORT #{INT LONG FLOAT DOUBLE}
   CHAR #{INT LONG FLOAT DOUBLE}
   INT #{LONG FLOAT DOUBLE}
   LONG #{FLOAT DOUBLE}
   FLOAT #{DOUBLE}})

(def ^:private narrower-primitive-types
  {SHORT #{BYTE CHAR}
   CHAR #{BYTE SHORT}
   INT #{BYTE SHORT CHAR}
   LONG #{BYTE SHORT CHAR INT}
   FLOAT #{BYTE SHORT CHAR INT LONG}
   DOUBLE #{BYTE SHORT CHAR INT LONG FLOAT}})

(defn- proper-primitive-super? [t1 t2]
  (boolean (get-in narrower-primitive-types [t1 t2])))

(def ^:private CLONEABLE (Type/getType Cloneable))
(def ^:private SERIALIZABLE (Type/getType java.io.Serializable))

(defn- proper-reference-super? [cenv t1 t2]
  (or (= t1 OBJECT)
      (= t2 nil)
      (and (not= t1 nil)
           (if (array-type? t2)
             (or (#{OBJECT CLONEABLE SERIALIZABLE} t1)
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
                     (contains? (supers c) c1)))))))))

(defn super? [cenv t1 t2]
  (or (= t1 t2)
      (case [(primitive-type? t1) (primitive-type? t2)]
        [true  true ] (proper-primitive-super? t1 t2)
        [false false] (proper-reference-super? cenv t1 t2)
        false)))

(defn ^Type object-type [obj]
  (cond (boolean? obj) BOOLEAN
        (char? obj) CHAR
        (integer? obj) INT
        (or (float? obj) (decimal? obj)) DOUBLE
        (string? obj) STRING
        (instance? Type obj) CLASS
        :else nil))

(defn type-category ^long [t]
  (if (#{LONG DOUBLE} t) 2 1))

(defn widening-primitive-conversion [from to]
  (when (get-in wider-primitive-types [from to])
    {:conversion :widening-primitive :from from :to to}))

(defn narrowing-primitive-conversion [from to]
  (when (get-in narrower-primitive-types [from to])
    {:conversion :narrowing-primitive :from from :to to}))

(def ^:private boxed-types
  {BOOLEAN BOOLEAN_CLASS
   BYTE BYTE_CLASS
   CHAR CHARACTER_CLASS
   SHORT SHORT_CLASS
   INT INTEGER_CLASS
   LONG LONG_CLASS
   FLOAT FLOAT_CLASS
   DOUBLE DOUBLE_CLASS})

(def ^:private unboxed-types
  (into {} (map (fn [[k v]] [v k])) boxed-types))

(defn convertible-to-integral? [t]
  (or (integral-type? t)
      (some-> (unboxed-types t) integral-type?)))

(defn convertible-to-numeric? [t]
  (or (numeric-type? t)
      (some-> (unboxed-types t) numeric-type?)))

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
  (when (and (not (primitive-type? from))
             (not (primitive-type? to))
             (not (proper-reference-super? cenv to from))
             (or  (proper-reference-super? cenv from to)
                  (and (array-type? to)
                      (or (#{OBJECT CLONEABLE SERIALIZABLE} from)
                          (and (array-type? from)
                               (let [e1 (element-type from) e2 (element-type to)]
                                 (narrowing-reference-conversion cenv e1 e2)))))
                  (case [(boolean (some-> (type->class from) (.isInterface)))
                         (boolean (some-> (type->class to) (.isInterface)))]
                    [false true ] (not (final-type? cenv from))
                    [true  false] (not (final-type? cenv to))
                    [true  true]  true
                    false)))
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
                        [{:conversion :narrowing-reference :from from :to (:to box)}
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

    #{INT LONG DOUBLE}
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

(defn- walk-class-hierarchy [^Class class f]
  ;; Here we assume that a JiSE class belongs to a package other than
  ;; any package a Java class belongs to, so JiSE classes can't refer to
  ;; any non-public Java classes
  (letfn [(walk [^Class c]
            (when c
              (concat (when (Modifier/isPublic (.getModifiers c))
                        (f c))
                      (mapcat walk (.getInterfaces c))
                      (walk (.getSuperclass c)))))]
   (walk class)))

(defn- accessible-from? [cenv caller class access]
  (or (:public access)
      (and (or (:private access) (:package access)) (= caller class))
      (and (:protected access) (super? cenv class caller))))

(defn find-field [cenv caller class name]
  (let [class-name (type->symbol class)
        name' (munge name)]
    (letfn [(field->map [c ^Field f]
              (let [type (class->type (.getType f))
                    access (modifiers->access-flags (.getModifiers f))]
                (when (accessible-from? cenv caller (class->type c) access)
                  (cond-> {:class (class->type (.getDeclaringClass f))
                           :type type
                           :access access}
                    (and (:static access) (:final access)
                         (or (primitive-type? type) (= type STRING)))
                    (assoc :value (.get f nil))))))
            (walk [^Class c]
              (-> (walk-class-hierarchy c
                    (fn [^Class c]
                      (some->> (.getDeclaredFields c)
                               (filter #(= (.getName ^Field %) name'))
                               first
                               (field->map c)
                               vector)))
                  first))]
      (if-let [entry (get-in cenv [:classes class-name])]
        (if-let [field (get-in entry [:fields name])]
          (when (accessible-from? cenv caller class (:access field))
            (assoc field :class class))
          ;; Here we assume all the superclasses and interfaces are defined outside of JiSE
          (let [{:keys [parent interfaces]} entry]
            (or (some walk (map type->class interfaces))
                (walk (type->class parent)))))
        (walk (type->class class))))))

(defn- remove-overridden-methods [cenv methods]
  (->> methods
       (reduce (fn [ms {:keys [param-types] :as m}]
                 (if-let [m' (get ms param-types)]
                   (cond-> ms (super? cenv (:class m') (:class m)) (assoc param-types m))
                   (assoc ms param-types m)))
               {})
       vals))

(defn- params-compatible? [nargs nparams varargs?]
  (or (= nargs nparams)
      (and varargs? (>= nargs (dec nparams)))))

(defn get-methods [cenv caller class name]
  (let [class-name (type->symbol class)
        name' (munge name)]
    (letfn [(method->map [^Class c ^Method m]
              (let [access (modifiers->access-flags (.getModifiers m))]
                (when (accessible-from? cenv caller (class->type c) access)
                  {:class (class->type (.getDeclaringClass m))
                   :interface? (.isInterface c)
                   :param-types (mapv class->type (.getParameterTypes m))
                   :return-type (class->type (.getReturnType m))
                   :access (cond-> access (.isVarArgs m) (conj :varargs))})))
            (walk [^Class c]
              (walk-class-hierarchy c
                (fn [^Class c]
                  (keep (fn [^Method m]
                          (when (= (.getName m) name')
                            (method->map c m)))
                        (.getDeclaredMethods c)))))]
      (->> (if-let [entry (get-in cenv [:classes class-name])]
             (concat (->> (get-in entry [:methods name])
                          (keep (fn [{:keys [access] :as m}]
                                  (when (accessible-from? cenv caller class access)
                                    (assoc m :class class)))))
                     (mapcat (comp walk type->class) (:interfaces entry))
                     (walk (type->class (:parent entry))))
             (let [c (type->class class)]
               (cond-> (walk c) (.isInterface c) (concat (walk Object)))))
           (remove-overridden-methods cenv)))))

(defn- convert-arg-types-with [f param-types arg-types throws-on-failure?]
  (let [fail (if throws-on-failure?
               #(->> {:arg-type %1 :param-type %2}
                     (ex-info "arg type mismatch")
                     (throw))
               (fn [_ _] (reduced nil)))]
    (->> (map vector arg-types param-types)
         (reduce (fn [acc [at pt]]
                   (if-let [cs (f at pt)]
                     (conj acc cs)
                     (fail at pt)))
                 []))))

(defn strict-invocation-conversion
  [cenv arg-types method & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
  (letfn [(f [from to]
            (if (= from to)
              []
              (when-let [c (or (widening-primitive-conversion from to)
                               (widening-reference-conversion cenv from to))]
                [c])))]
    (when-let [cs (convert-arg-types-with f (:param-types method) arg-types
                                          throws-on-failure?)]
      (assoc method :conversions cs))))

(defn loose-invocation-conversion
  [cenv arg-types method & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
  (when-let [cs (convert-arg-types-with (partial assignment-conversion cenv)
                                        (:param-types method)
                                        arg-types
                                        throws-on-failure?)]
    (assoc method :conversions cs)))

(defn variable-arity-invocation-conversion
  [cenv arg-types {:keys [param-types] :as method}
   & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
  (let [required-param-types (butlast param-types)
        vararg-type (last param-types)]
    (when-let [cs (convert-arg-types-with (partial assignment-conversion cenv)
                                          required-param-types
                                          arg-types
                                          throws-on-failure?)]
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
                                          (drop (dec nparams) arg-types)
                                          throws-on-failure?)
              (assoc method :conversions cs)))))))

(defn- maximally-specific-methods [cenv methods]
  (for [[i m1] (map-indexed vector methods)
        :when (not-any? (fn [[j m2]]
                          (when-not (= i j)
                            (->> (map vector (:param-types m1) (:param-types m2))
                                 (every? (fn [[p1 p2]] (super? cenv p1 p2))))))
                        (map-indexed vector methods))]
    m1))

(defn- filter-methods [cenv arg-types methods]
  (let [{fixed-ms false, variable-ms true} (group-by #(boolean (:varargs (:access %))) methods)
        filter-with (fn [f ms]
                      (->> ms
                           (keep #(f cenv arg-types % :throws-on-failure? false))
                           seq))]
    (some->> (or (filter-with strict-invocation-conversion fixed-ms)
                 (filter-with loose-invocation-conversion fixed-ms)
                 (filter-with variable-arity-invocation-conversion variable-ms))
             (maximally-specific-methods cenv))))

(defn- ensure-not-empty
  ([cause message methods]
   (ensure-not-empty cause message {} methods))
  ([cause message info methods]
   (when (empty? methods)
     (throw (ex-info message (assoc info :cause cause))))
   methods))

(defn find-methods
  ([caller class name arg-types]
   (find-methods {} caller class name arg-types))
  ([cenv caller class name arg-types
    & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
   (let [nargs (count arg-types)
         ensure-not-empty (if throws-on-failure? ensure-not-empty #(last %&))]
     (as-> (get-methods cenv caller class name) ms
       (ensure-not-empty :no-such-target "no such method" ms)
       (->> (filter (fn [{:keys [param-types access]}]
                      (params-compatible? nargs (count param-types) (:varargs access)))
                    ms)
            (ensure-not-empty :args-length-mismatch "args length mismatch" {:alternatives ms}))
       (->> (filter-methods cenv arg-types ms)
            (ensure-not-empty :arg-type-mismatch "arg type mismatch" {:alternatives ms}))
       (seq ms)))))

(defn get-ctors [cenv caller class]
  (let [class-name (type->symbol class)]
    (or (->> (get-in cenv [:classes class-name :ctors])
             (filter (fn [{:keys [access]}]
                       (accessible-from? cenv caller class access)))
             seq)
        (->> (.getDeclaredConstructors (type->class class))
             (keep (fn [^Constructor ctor]
                     (let [access (modifiers->access-flags (.getModifiers ctor))
                           varargs? (.isVarArgs ctor)]
                       (when (accessible-from? cenv caller class access)
                         {:param-types (mapv class->type (.getParameterTypes ctor))
                          :access (cond-> access varargs? (conj :varargs))}))))
             seq))))

(defn find-ctors
  ([caller class arg-types]
   (find-ctors {} caller class arg-types))
  ([cenv caller class arg-types
    & {:keys [throws-on-failure?] :or {throws-on-failure? true}}]
   (let [nargs (count arg-types)]
     (as-> (get-ctors cenv caller class) ms
       (ensure-not-empty :no-such-target "no such ctor" ms)
       (->> (filter (fn [{:keys [param-types access]}]
                      (params-compatible? nargs (count param-types) (:varargs access)))
                    ms)
            (ensure-not-empty :args-length-mismatch "args length mismatch" {:alternatives ms}))
       (->> (filter-methods cenv arg-types ms)
            (ensure-not-empty :arg-type-mismatch "arg type mismatch" {:alternatives ms}))
       (seq ms)))))
