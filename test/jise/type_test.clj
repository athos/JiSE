(ns jise.type-test
  (:require [clojure.test :refer [deftest testing is are]]
            [jise.type :as t])
  (:import [clojure.asm Type]))

(import 'java.io.BufferedReader)

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
      (t/tag->type '[Object]) (t/tag->type '[String]))
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
      (t/tag->type '[long]) (t/tag->type '[int]))))
