(ns example.graph
  (:require [jise.core :refer [defclass]]))

;; This example code is taken from [JLS §14.16](https://docs.oracle.com/javase/specs/jls/se12/html/jls-14.html#jls-14.16)

^:public
(defclass Graph
  ^:public ^{:tag [[int]]}
  (def edges)

  ^:public
  (defm Graph [^{:tag [[int]]} edges]
    (set! (.-edges this) edges))

  ^:public ^Graph
  (defm loseEdges [^int i ^int j]
    (let [n (alength edges)
          new-edges (new [[int]] n)]
      ^{:label :edgelist}
      (for [k 0 (< k n) (inc! k)]
        (let [z 0]
          ^{:label :search}
          (do (cond (== k i)
                    (for [nil (< z (alength (edges k))) (inc! z)]
                      (when (== (edges k z) j)
                        (break :search)))
                    (== k j)
                    (for [nil (< z (alength (edges k))) (inc! z)]
                      (when (== (edges k z) i)
                        (break :search))))
              (set! (new-edges k) (edges k))
              (continue :edgelist))
          (let [m (- (alength (edges k)) 1)
                ne (new [int] m)]
            (System/arraycopy (edges k) 0 ne 0 z)
            (System/arraycopy (edges k) (+ z 1) ne z (- m z))
            (set! (new-edges k) ne))))
      (Graph. new-edges))))

(comment

  (require '[jise.utils :as jise])
  (def g (Graph. (jise/do (new [[int]] [[1 2 3] [3] [3] [0 2]]))))
  (def g' (.loseEdges g 0 3))
  (mapv vec (.-edges g')) ;=> [[1 2] [3] [3] [2]]

  )
