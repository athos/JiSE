# JiSE: Java in S-Expression
[![Clojars Project](https://img.shields.io/clojars/v/jise.svg)](https://clojars.org/jise)
[![CircleCI](https://circleci.com/gh/athos/JiSE.svg?style=shield)](https://circleci.com/gh/athos/JiSE)

JiSE is Clojure's embedded DSL for making it as easy (or maybe even easier?) to write imperative code than in Java.

## Usage

```clojure
(require '[jise.core :refer [defclass]])


^:public
(defclass Counter
  ^:private
  (def ^int c)
  ^:public
  (defm Counter [^int c]
    (set! (.-c this) c))
  ^:public ^int
  (defm inc []
    (inc! (.-c this))))

;; You can use the defined class (`Counter`) as an ordinary Java class
(def c (Counter. 10))
(.inc c) ;=> 11
(.inc c) ;=> 12
(.inc c) ;=> 13

;; Also, you can even write quite imperative code easily as follows:

^:public
(defclass Qsort
  ^:public ^:static
  (defm qsort [^{:tag [int]} xs]
    (qsort xs 0 (- (alength xs) 1)))

  ^:private ^:static
  (defm qsort [^{:tag [int]} xs ^int left ^int right]
    (when (< left right)
      (let [p (aget xs (/ (+ left right) 2))
            l left
            r right]
        (while (<= l r)
          (while (< (aget xs l) p) (inc! l))
          (while (> (aget xs r) p) (dec! r))
          (when (<= l r)
            (let [tmp (aget xs l)]
              (aset xs l (aget xs r))
              (aset xs r tmp)
              (inc! l)
              (dec! r))))
        (qsort xs left r)
        (qsort xs l right)))))

(def arr (int-array [3 1 4 1 5 9 2]))
(Qsort/qsort arr)
(seq arr)
;=> (1 1 2 3 4 5 9)
```

## License

Copyright © 2019 Shogo Ohta

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
