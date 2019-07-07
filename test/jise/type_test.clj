(ns jise.type-test
  (:require [clojure.test :refer [deftest testing is are]]
            [jise.type :as t])
  (:import [clojure.asm Type]
           [java.lang.reflect Modifier]
           [java.io BufferedReader]))

(deftest tag->type-test
  (testing "symbol representing primitive type can be converted to corresponding Type"
    (are [t type] (= type (t/tag->type t))
      'boolean t/BOOLEAN
      'byte t/BYTE
      'short t/SHORT
      'char t/CHAR
      'int t/INT
      'long t/LONG
      'float t/FLOAT
      'double t/DOUBLE))
  (testing "symbol representing Java classe can be converted to corresponding Type"
    (are [t class]
        (= (Type/getType ^Class class)
           (binding [*ns* (the-ns 'jise.type-test)]
             (t/tag->type t)))
      'String
      String

      'java.util.List
      java.util.List

      'BufferedReader
      BufferedReader))
  (testing "vector representing array type can be converted to corresponding Type"
    (are [t type]
        (= (Type/getType ^String type) (t/tag->type t))
      '[int]
      "[I"

      '[Object]
      "[Ljava/lang/Object;"

      '[[double]]
      "[[D"))
  (testing "symbol representing vararg type can be converted to corresponding array Type"
    (are [t type]
        (= (Type/getType ^String type)
           (binding [*ns* (the-ns 'jise.type-test)]
            (t/tag->type {} t :allow-vararg-param-type? true)))
      'int...
      "[I"

      'BufferedReader...
      "[Ljava/io/BufferedReader;"))
  (testing "symbol representing user-defined class can be converted to corresponding Type"
    (is (= (Type/getType "Lfoo/bar/C;")
           (t/tag->type {:classes {'foo.bar.C {}}} 'foo.bar.C)))
    (is (= (Type/getType "Lfoo/bar/C;")
           (t/tag->type {:classes {'foo.bar.C {}}
                         :aliases {'C 'foo.bar.C}}
                        'C))))
  (testing "symbol representing unknown type cannot be converted to Type"
    (is (thrown-with-msg? Exception #"cannot resolve type UnknownClass"
                          (t/tag->type 'UnknownClass)))
    (is (nil? (t/tag->type {} 'UnknownClass :throws-on-failure? false))))
  (testing "vararg type is not allowed unless it's especially allowed"
    (is (thrown-with-msg? Exception #"vararg param type not allowed"
                          (t/tag->type 'String...)))))

(deftest type->class-test
  (are [type class]
      (= class (t/type->class (Type/getType ^String type)))
    "Ljava/lang/String;"
    String

    "[I"
    (Class/forName "[I")))

(deftest type->tag-test
  (are [type t]
      (= t (t/type->tag type))
    t/BOOLEAN
    'boolean

    t/OBJECT
    'java.lang.Object

    (Type/getType "[I")
    '[int]

    (Type/getType "[[Ljava/lang/String;")
    '[[java.lang.String]]))

(deftest super?-test
  (testing "t1 is super type of t2"
    (are [t1 t2] (t/super? {} t1 t2)
      t/SHORT t/BYTE
      t/INT t/SHORT
      t/INT t/CHAR
      t/LONG t/INT
      t/FLOAT t/LONG
      t/DOUBLE t/FLOAT
      t/DOUBLE t/CHAR
      t/OBJECT t/STRING
      (t/tag->type 'java.io.Closeable) (t/tag->type 'java.io.Reader)
      (t/tag->type 'Cloneable) (t/tag->type '[int])
      (t/tag->type '[Object]) (t/tag->type '[String])
      t/STRING nil)
    (let [cenv {:classes {'foo.bar.C
                          {:parent t/OBJECT
                           :interfaces #{(t/tag->type 'Runnable)}}}}]
      (are [t1 t2] (t/super? cenv t1 t2)
        (t/tag->type 'Runnable)
        (t/tag->type cenv 'foo.bar.C))))
  (testing "t1 is not super type of t2"
    (are [t1 t2] (not (t/super? {} t1 t2))
      t/BOOLEAN t/INT
      t/INT t/BOOLEAN
      (t/tag->type '[long]) (t/tag->type '[int])
      nil t/STRING)))

(deftest modifiers->access-flags-test
  (are [ms expected]
      (= expected (t/modifiers->access-flags ms))
    (bit-or Modifier/ABSTRACT Modifier/PUBLIC)
    #{:abstract :public}

    (bit-or Modifier/FINAL Modifier/PRIVATE Modifier/STATIC)
    #{:final :private :static}

    (bit-or Modifier/PROTECTED Modifier/TRANSIENT)
    #{:protected :transient}

    Modifier/VOLATILE
    #{:package :volatile}))

(deftest widening-primitive-conversion-test
  (testing "t1 can be widened to t2"
    (are [t1 t2]
        (= {:conversion :widening-primitive :from t1 :to t2}
           (t/widening-primitive-conversion t1 t2))
      t/BYTE t/INT
      t/INT t/LONG
      t/LONG t/FLOAT
      t/FLOAT t/DOUBLE))
  (testing "t1 cannot be widened to t2"
    (are [t1 t2]
        (= nil (t/widening-primitive-conversion t1 t2))
      t/BOOLEAN t/INT
      t/INT t/SHORT
      t/DOUBLE t/FLOAT
      t/CHAR t/STRING
      t/STRING t/CHAR)))

(deftest narrowing-primitive-conversion-test
  (testing "t1 can be narrowed to t2"
    (are [t1 t2]
        (= {:conversion :narrowing-primitive :from t1 :to t2}
           (t/narrowing-primitive-conversion t1 t2))
      t/INT t/CHAR
      t/LONG t/SHORT
      t/FLOAT t/LONG
      t/DOUBLE t/FLOAT))
  (testing "t1 cannot be narrowed to t2"
    (are [t1 t2]
        (= nil (t/narrowing-primitive-conversion t1 t2))
      t/INT t/BOOLEAN
      t/CHAR t/INT
      t/FLOAT t/DOUBLE
      t/CHAR t/STRING
      t/STRING t/CHAR)))

(deftest boxing-conversion-test
  (testing "t1 can be boxed to t2"
    (are [t1 t2]
        (= {:conversion :boxing :from t1 :to t2}
           (t/boxing-conversion t1))
      t/BOOLEAN t/BOOLEAN_CLASS
      t/BYTE t/BYTE_CLASS
      t/CHAR t/CHARACTER_CLASS
      t/SHORT t/SHORT_CLASS
      t/INT t/INTEGER_CLASS
      t/LONG t/LONG_CLASS
      t/FLOAT t/FLOAT_CLASS
      t/DOUBLE t/DOUBLE_CLASS))
  (testing "t cannot be boxed"
    (are [t] (= nil (t/boxing-conversion t))
      t/BOOLEAN_CLASS
      t/STRING
      (t/tag->type '[int]))))

(deftest unboxing-conversion-test
  (testing "t1 can be unboxed to t2"
    (are [t1 t2]
        (= {:conversion :unboxing :from t1 :to t2}
           (t/unboxing-conversion t1))
      t/BOOLEAN_CLASS t/BOOLEAN
      t/BYTE_CLASS t/BYTE
      t/CHARACTER_CLASS t/CHAR
      t/SHORT_CLASS t/SHORT
      t/INTEGER_CLASS t/INT
      t/LONG_CLASS t/LONG
      t/FLOAT_CLASS t/FLOAT
      t/DOUBLE_CLASS t/DOUBLE))
  (testing "t cannot be unboxed"
    (are [t] (= nil (t/unboxing-conversion t))
      t/BOOLEAN
      t/STRING
      (t/tag->type '[Integer]))))

(deftest widening-reference-conversion-test
  (testing "t1 can be widened to t2"
    (are [t1 t2]
        (= {:conversion :widening-reference :from t1 :to t2}
           (binding [*ns* (the-ns 'jise.type-test)]
             (t/widening-reference-conversion {} t1 t2)))
      t/STRING t/OBJECT
      (t/tag->type 'java.io.BufferedReader) (t/tag->type 'java.io.Closeable)
      (t/tag->type '[String]) (t/tag->type '[Object])
      nil t/STRING)
    (let [r (t/tag->type 'java.io.Reader)
          cenv {:classes {'foo.bar.C {:parent r}}}
          c (t/tag->type cenv 'foo.bar.C)]
      (is (= {:conversion :widening-reference :from c :to r}
             (t/widening-reference-conversion cenv c r)))))
  (testing "t1 cannot be widened to t2"
    (are [t1 t2] (= nil (t/widening-reference-conversion {} t1 t2))
      t/INT t/LONG
      t/OBJECT t/STRING
      t/STRING (t/tag->type 'java.io.Closeable)
      t/STRING nil)))

(deftest narrowing-reference-conversion-test
  (testing "t1 can be narrowed to t2"
    (are [t1 t2]
        (= {:conversion :narrowing-reference :from t1 :to t2}
           (t/narrowing-reference-conversion {} t1 t2))
      t/OBJECT t/STRING
      (t/tag->type 'java.io.Closeable) (t/tag->type 'java.io.BufferedReader)
      (t/tag->type '[Object]) (t/tag->type '[String])
      t/STRING nil)
    (let [r (t/tag->type 'java.io.Reader)
          cenv {:classes {'foo.bar.C {:parent r}}}
          c (t/tag->type cenv 'foo.bar.C)]
      (is (= {:conversion :narrowing-reference :from r :to c}
             (t/narrowing-reference-conversion cenv r c)))))
  (testing "t1 cannot be narrowed to t2"
    (are [t1 t2]
        (= nil (t/narrowing-reference-conversion {} t1 t2))
      t/LONG t/INT
      t/STRING t/OBJECT
      ;(t/tag->type 'java.io.Closeable) t/STRING
      nil t/STRING)))

(deftest assignment-conversion-test
  (testing "t1 can be converted to t2 in assignment context"
    (are [t1 t2 expected]
        (= expected (t/assignment-conversion {} t1 t2))
      t/INT t/INT
      []

      t/INT t/LONG
      [{:conversion :widening-primitive :from t/INT :to t/LONG}]

      t/INT t/INTEGER_CLASS
      [{:conversion :boxing :from t/INT :to t/INTEGER_CLASS}]

      t/CHAR t/OBJECT
      [{:conversion :boxing :from t/CHAR :to t/CHARACTER_CLASS}
       {:conversion :widening-reference :from t/CHARACTER_CLASS :to t/OBJECT}]

      t/BOOLEAN_CLASS t/BOOLEAN
      [{:conversion :unboxing :from t/BOOLEAN_CLASS :to t/BOOLEAN}]

      t/CHARACTER_CLASS t/DOUBLE
      [{:conversion :unboxing :from t/CHARACTER_CLASS :to t/CHAR}
       {:conversion :widening-primitive :from t/CHAR :to t/DOUBLE}]

      t/STRING t/OBJECT
      [{:conversion :widening-reference :from t/STRING :to t/OBJECT}]

      nil t/STRING
      [{:conversion :widening-reference :from nil :to t/STRING}])
    (let [r (t/tag->type 'java.io.Reader)
          cenv {:classes {'foo.bar.C {:parent r}}}
          c (t/tag->type cenv 'foo.bar.C)]
      (is (= [{:conversion :widening-reference :from c :to r}]
             (t/assignment-conversion cenv c r)))))
  (testing "t1 cannot be converted to t2 in assignment context"
    (are [t1 t2] (= nil (t/assignment-conversion {} t1 t2))
      t/LONG t/INT
      t/INT t/LONG_CLASS
      t/FLOAT_CLASS t/INT
      t/OBJECT t/STRING
      t/STRING nil)))

(deftest casting-conversion-test
  (testing "t1 can be converted to t2 in casting context"
    (are [t1 t2 expected]
        (= expected (t/casting-conversion {} t1 t2))
      t/INT t/INT
      []

      t/INT t/LONG
      [{:conversion :widening-primitive :from t/INT :to t/LONG}]

      t/DOUBLE t/LONG
      [{:conversion :narrowing-primitive :from t/DOUBLE :to t/LONG}]

      t/CHAR t/CHARACTER_CLASS
      [{:conversion :boxing :from t/CHAR :to t/CHARACTER_CLASS}]

      t/FLOAT t/OBJECT
      [{:conversion :boxing :from t/FLOAT :to t/FLOAT_CLASS}
       {:conversion :widening-reference :from t/FLOAT_CLASS :to t/OBJECT}]

      t/LONG_CLASS t/LONG
      [{:conversion :unboxing :from t/LONG_CLASS :to t/LONG}]

      t/OBJECT t/CHAR
      [{:conversion :narrowing-reference :from t/OBJECT :to t/CHARACTER_CLASS}
       {:conversion :unboxing :from t/CHARACTER_CLASS :to t/CHAR}]

      t/STRING t/OBJECT
      [{:conversion :widening-reference :from t/STRING :to t/OBJECT}]

      (t/tag->type 'java.io.Reader)
      (t/tag->type 'java.io.BufferedReader)
      [{:conversion :narrowing-reference
        :from (t/tag->type 'java.io.Reader)
        :to (t/tag->type 'java.io.BufferedReader)}]

      nil t/STRING
      [{:conversion :widening-reference :from nil :to t/STRING}]

      t/STRING nil
      [{:conversion :narrowing-reference :from t/STRING :to nil}])
    (let [r (t/tag->type 'java.io.Reader)
          cenv {:classes {'foo.bar.C {:parent r}}}
          c (t/tag->type cenv 'foo.bar.C)]
      (is (= [{:conversion :narrowing-reference :from r :to c}]
             (t/casting-conversion cenv r c)))))
  (testing "t1 cannot be converted to t2 in casting context"
    (are [t1 t2] (= nil (t/casting-conversion {} t1 t2))
      t/INT t/LONG_CLASS
      t/CHAR nil
      t/FLOAT_CLASS t/INT
      nil t/BOOLEAN)))
