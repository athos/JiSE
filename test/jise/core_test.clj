(ns jise.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [jise.core :refer [defclass]])
  (:import clojure.lang.Compiler$CompilerException))

(defn- eval-expr [type expr]
  (eval `(do
           ^:public
           (defclass ~'C
             ~(with-meta
                `(~'defm ~'m [] ~expr)
                {:tag type :public true}))
           (.m (C.)))))

(deftest literal-test
  (testing "valid literals"
    (are [expr type expected] (= expected (eval-expr 'type 'expr))
      true boolean true
      false boolean false
      \a char \a
      42 int 42
      42.195 double 42.195
      "foo" String "foo"
      nil Object nil))
  (testing "invalid literals"
    (are [expr] (thrown? Compiler$CompilerException (eval-expr 'Object 'expr))
      :a
      [0]
      {"foo" 0}
      #{"bar"}
      #"regex")))

(deftest arithmetic-test
  (testing "simple arithmetics"
    (are [expr type expected] (= expected (eval-expr 'type 'expr))
      (+ 1 2) int 3
      (+ 1.0 2.0) double 3.0
      (+ 1 2 3) int 6
      (+ 100) int 100
      (+) int 0
      (- 3 1) int 2
      (- 3.0 1.0) double 2.0
      (- 3 2 1) int 0
      (- 100) int -100
      (* 10 20) int 200
      (* 2.0 2.0) double 4.0
      (* 2.0) double 2.0
      (*) int 1
      (/ 9 2) int 4
      (/ 9.0 2.0) double 4.5
      (/ 2.0) double 0.5
      (% 9 2) int 1
      (% 9.0 2.0) double 1.0
      (& 5 3) int 1
      (& true true) boolean true
      (& true false) boolean false
      (| 5 3) int 7
      (| false false) boolean false
      (| false true) boolean true
      (xor 5 3) int 6
      (xor true true) boolean false
      (xor true false) boolean true
      (! -1) int 0
      (<< 5 2) int 20
      (>> 14 2) int 3
      (>> -11 2) int -3
      (>>> 14 2) int 3
      (>>> -11 2) int 0x3ffffffd))
  (testing "numeric promotion"
    (are [expr type expected] (= expected (eval-expr 'type 'expr))
      (+ 4.0 1) double 5.0
      (+ 2 (Long/valueOf 3)) Long 5
      (+ (Byte/valueOf "1") (Short/valueOf "2")) int 3
      (- 1 0.5) double 0.5
      (- \b 1) int 97
      (* (Short/valueOf "3") 3) int 9
      (* 3 (Float/valueOf "3.0")) float 9.0
      (/ 1.0 2) double 0.5
      (/ (Short/valueOf "9") 3) int 3
      (% 5 (Byte/valueOf "2")) int 1
      (% 9.0 2) double 1.0
      (& 5 (Long/valueOf "3")) long 1
      (& true (Boolean/valueOf false)) boolean false
      (| (Long/valueOf "5") 3) long 7
      (| (Boolean/valueOf true) false) boolean true
      (xor (Short/valueOf "5") (Byte/valueOf "3")) int 6
      (xor false (Boolean/valueOf true)) boolean true
      (! (Byte/valueOf "0")) int -1
      (<< (Long/valueOf "5") 2) long 20
      (>> (Long/valueOf "14") 2) long 3
      (>>> (Long/valueOf "-11") 2) long 0x3ffffffffffffffd))
  (testing "invalid arithmetics"
    (are [expr] (thrown? Compiler$CompilerException (eval-expr 'Object 'expr))
      (+ 1 true)
      (- false 2)
      (-)
      (* "foo" 3)
      (/ 4 true)
      (/)
      (% 9 true)
      (& 5.0 3)
      (& 1 true)
      (| 5 3.0)
      (| 1 true)
      (xor 5 3.0)
      (xor 1 true)
      (! 1.0)
      (<< 2.0 1)
      (<< 2 1.0)
      (>> 2.0 1)
      (>> 2 1.0)
      (>>> 2.0 1)
      (>>> 2 1.0))))

(deftest comparison-test
  (testing "comparison to 0"
    (are [expr expected] (= expected (eval-expr 'boolean 'expr))
      (== 0 0) true
      (== 0 1) false
      (== 1 0) false
      (!= 0 0) false
      (!= 0 1) true
      (!= 1 0) true
      (< 0 0) false
      (< 0 1) true
      (< 1 0) false
      (> 0 0) false
      (> 0 1) false
      (> 1 0) true
      (<= 0 0) true
      (<= 0 1) true
      (<= 1 0) false
      (>= 0 0) true
      (>= 0 1) false
      (>= 1 0) true))
  (testing "simple comparison"
    (are [expr expected] (= expected (eval-expr 'boolean 'expr))
      (== true true) true
      (== true false) false
      (== \a \a) true
      (== \a \b) false
      (== 1.0 1.0) true
      (== 1.0 2.0) false
      (== "foo" "foo") true
      (== "foo" "bar") false
      (== "foo" nil) false
      (== nil nil) true
      (== 1 1 1) true
      (== 1 1 2) false
      (!= true true) false
      (!= true false) true
      (!= \a \a) false
      (!= \a \b) true
      (!= 1.0 1.0) false
      (!= 1.0 2.0) true
      (!= "foo" "foo") false
      (!= "foo" "bar") true
      (!= "foo" nil) true
      (!= nil nil) false
      (!= 1 1 1) false
      (!= 1 1 2) true
      (< 100 100) false
      (< 1.0 1.0) false
      (< 100 200) true
      (< 1.0 2.0) true
      (< 200 100) false
      (< 2.0 1.0) false
      (< 1 2 3) true
      (< 1 2 2) false
      (< 1 3 2) false
      (> 100 100) false
      (> 1.0 1.0) false
      (> 100 200) false
      (> 1.0 2.0) false
      (> 200 100) true
      (> 2.0 1.0) true
      (> 3 2 1) true
      (> 3 2 2) false
      (> 2 3 1) false
      (<= 100 100) true
      (<= 1.0 1.0) true
      (<= 100 200) true
      (<= 1.0 2.0) true
      (<= 200 100) false
      (<= 2.0 1.0) false
      (<= 1 2 3) true
      (<= 1 2 2) true
      (<= 1 3 2) false
      (>= 100 100) true
      (>= 100 200) false
      (>= 1.0 2.0) false
      (>= 200 100) true
      (>= 2.0 1.0) true
      (>= 3 2 1) true
      (>= 3 2 2) true
      (>= 2 3 1) false))
  (testing "numeric promotion"
    (are [expr expected] (= expected (eval-expr 'boolean 'expr))
      (== \a 97) true
      (== 1 1.0) true
      (== (Long/valueOf "1") 1) true
      (== \a 97 (Long/valueOf "97")) true
      (!= \a 97) false
      (!= 1 1.0) false
      (!= (Long/valueOf "1") 1) false
      (!= \a 97 (Long/valueOf "97")) false
      (< 97 \a) false
      (< \a 98) true
      (< 1 2.0) true
      (< (Long/valueOf "1") 2) true
      (< \a 98 (Long/valueOf "99")) true
      (> \a 97) false
      (> 98 \a) true
      (> 1 2.0) false
      (> (Long/valueOf "1") 2) false
      (> \a 96 (Long/valueOf "95")) true
      (<= 97 \a) true
      (<= \a 98) true
      (<= 1 2.0) true
      (<= (Long/valueOf "1") 2) true
      (<= \a 98 (Long/valueOf "99")) true
      (>= 97 \a) true
      (>= \a 98) false
      (>= 1 2.0) false
      (>= (Long/valueOf "1") 2) false
      (>= \a 96 (Long/valueOf "96")) true))
  (testing "invalid comparison"
    (are [expr] (thrown? Compiler$CompilerException (eval-expr 'Object 'expr))
      (== true 1)
      (== 1.0 "foo")
      (== \a nil)
      (== (Integer/valueOf 1) (Long/valueOf "1"))
      (== 1)
      (!= true 1)
      (!= 1.0 "foo")
      (!= \a nil)
      (!= (Integer/valueOf 1) (Long/valueOf "1"))
      (!= 1)
      (< true 1)
      (< 1.0 "foo")
      (< 1)
      (> true 1)
      (> 1.0 "foo")
      (> 1)
      (<= true 1)
      (<= 1.0 "foo")
      (<= 1)
      (>= true 1)
      (>= 1.0 "foo")
      (>= 1))))

(deftest casting-test
  (testing "valid casting"
    (are [expr type expected] (= expected (eval-expr 'type 'expr))
      (byte 42) byte 42
      (short 42) short 42
      (int 42) int 42
      (float 42) float 42.0
      (double 42) double 42.0
      (boolean (Boolean/valueOf "true")) boolean true
      (char 97) char \a
      (int \a) int 97
      (long (Long/valueOf "42")) long 42
      (cast long 42) long 42
      (cast Object "foo") Object "foo"
      (cast Object 42) Object 42
      (cast String (cast Object "foo")) String "foo"))
  (testing "invalid casting"
    (are [expr] (thrown? Compiler$CompilerException (eval-expr 'Object 'expr))
      (boolean 42)
      (byte true)
      (short false)
      (int nil)
      (float "foo")
      (double true)
      (cast String 42)
      (cast int "foo"))))

(deftest do-test
  (are [expr type expected] (= expected (eval-expr 'type 'expr))
    (do 42) int 42
    (do 43 42) int 42
    (do) Object nil))

(deftest let-test
  (testing "valid let expr"
    (are [expr type expected] (= expected (eval-expr 'type 'expr))
      (let [x 2] x) int 2
      (let [x 2 y 3] (+ x y)) int 5
      (let [x 3 x (+ x x)] x) int 6
      (let [x 3 y 5]
        (let [x (* x x)]
          (+ x y)))
      int 14

      (let [x 42]
        (- x 1)
        (+ x 1))
      int 43

      (let [x 100
            x (let [x 2] (+ x 1))]
        (+ x 1))
      int 4

      (do (let [x 100] x)
          (let [x 42] x))
      int 42

      (let [this 42] this)
      int 42

      (let [^int c \a] (+ c 1)) int 98))
  (testing "invalid let expr"
    (are [expr] (thrown? Compiler$CompilerException (eval-expr 'Object 'expr))
      (let [y x x 5] y)
      (let [x 0]
        (let [y 1] (+ x y))
        y)
      (let [^int x true] x))))

(deftest set!-test
  (testing "valid set! expr"
    (are [expr type expected] (= expected (eval-expr 'type 'expr))
      (let [x 42]
        (set! x (+ x 1))
        x)
      int 43

      (let [x 0]
        (set! x 42))
      int 42

      (let [x 0]
        (set! x \a)
        x)
      int 97))
  (testing "invalid set! expr"
    (are [expr] (thrown? Compiler$CompilerException (eval-expr 'Object 'expr))
      (set! x 0)
      (let [x 0]
        (set! x "foo"))
      (let [x 0]
        (set! x (long 42)))
      (let [^:final x 0]
        (set! x 1))
      (set! this this))))
