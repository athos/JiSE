# JiSE: Java in S-Expression

JiSE is Clojure's embedded DSL for making it as easy (or maybe even easier?) to write imperative code than in Java.

## Usage

```clojure
(require '[jise.core :refer [defclass]])

^:public
(defclass C
  ^:public ^int
  (defm sumUpTo [^int n]
    (let [^int sum 0]
      (for [^int i 0, (< i n), (set! i (+ i 1))]
        (set! sum (+ sum i)))
      sum)))

;; you can use C as an ordinary Java class
(def c (C.))
(.sumUpTo c 10) ;=> 45
(.sumUpTo c 100) ;=> 4950
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
