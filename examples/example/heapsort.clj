(ns example.heapsort
  (:refer-clojure :exclude [swap!])
  (:require [jise.core :as j :refer [defclass]]))

(defmacro parent [i]
  `(j// (j/- ~i 1) 2))

(defmacro left-child [i]
  `(j/+ (j/* 2 ~i) 1))

(defmacro right-child [i]
  `(j/+ (j/* 2 ~i) 2))

(defmacro swap! [arr i j]
  `(j/let [tmp# (~arr ~i)]
     (j/set! (~arr ~i) (~arr ~j))
     (j/set! (~arr ~j) tmp#)))

^:public
(defclass Heapsort
  ^:public ^:static
  (defm sort [^ints arr]
    (let [end (- (alength arr) 1)]
      (heapify! arr)
      (while (> end 0)
        (swap! arr end 0)
        (dec! end)
        (siftdown! arr 0 end))))

  ^:private ^:static
  (defm heapify! [^ints arr]
    (let [end (- (alength arr) 1)]
      (for [start (parent end), (>= start 0), (dec! start)]
        (siftdown! arr start end))))

  ^:private ^:static
  (defm siftdown! [^ints arr ^int start ^int end]
    (let [root start]
      (while (<= (left-child root) end)
        (let [left (left-child root)
              right (right-child root)
              swap root]
          (when (< (arr swap) (arr left))
            (set! swap left))
          (when (and (<= right end) (< (arr swap) (arr right)))
            (set! swap right))
          (when (== swap root)
            (return))
          (swap! arr root swap)
          (set! root swap)))))
  )

(comment

  (def arr (int-array [3 1 4 1 5 9 2 6 5]))
  (Heapsort/sort arr)
  (seq arr)

  )
