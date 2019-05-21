(ns example.qsort
  (:gen-class)
  (:require [jise.core :refer [defclass]]))

^:public
(defclass Qsort
  ^:public ^:static
  (defm qsort [^ints xs]
    (qsort xs 0 (- (alength xs) 1)))

  ^:private ^:static
  (defm qsort [^ints xs ^int left ^int right]
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
        (qsort xs left r)
        (qsort xs l right)))))

(defn -main [& args]
  (let [arr (int-array (map #(Integer/parseInt %) args))]
    (Qsort/qsort arr)
    (apply prn arr)))
