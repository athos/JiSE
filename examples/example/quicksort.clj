(ns example.quicksort
  (:gen-class)
  (:require [jise.core :refer [defclass]]))

^:public
(defclass Quicksort
  ^:public ^:static
  (defm sort [^ints xs]
    (sort xs 0 (- (alength xs) 1)))

  ^:private ^:static
  (defm sort [^ints xs ^int left ^int right]
    (when (< left right)
      (let [p (aget xs (/ (+ left right) 2))
            l left
            r right]
        (while (<= l r)
          (while (< (xs l) p) (inc! l))
          (while (> (xs r) p) (dec! r))
          (when (<= l r)
            (let [tmp (aget xs l)]
              (set! (xs l) (xs r))
              (set! (xs r) tmp)
              (inc! l)
              (dec! r))))
        (sort xs left r)
        (sort xs l right)))))

(comment

  (def arr (int-array [3 1 4 1 5 9 2 6 5]))
  (Quicksort/sort arr)
  (seq arr)

  )
