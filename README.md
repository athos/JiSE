# JiSE: Java in S-Expression
[![Clojars Project](https://img.shields.io/clojars/v/jise.svg)](https://clojars.org/jise)
[![CircleCI](https://circleci.com/gh/athos/JiSE.svg?style=shield)](https://circleci.com/gh/athos/JiSE)

JiSE is a Clojure DSL library that compiles a Java-like language into JVM bytecode at macroexpansion time.

## Features

Using JiSE, you can:

- Write imperative code that is compiled to JVM bytecode as efficient as written in Java
  - You can use assignment, (nested) loops, (labeled) break/continue, etc.
- Define your own Java class in a cleaner way than using `gen-class` or `proxy`
- Combine JiSE code with Clojure in a function- or expression-level of granularity
- Find more errors at compile time due to its static typing
- Extend JiSE syntax with Clojure's ordinary macros
  - Existing Clojure macros can also be used seamlessly from JiSE, such as `->`, `..` and `with-open`

## Installation

Add the following to your project `:dependencies`:

[![Clojars Project](https://clojars.org/jise/latest-version.svg)](https://clojars.org/jise)

If you would rather use an unstable version of the library via [Clojure CLI tool](https://clojure.org/guides/deps_and_cli), add the following to your `deps.edn` instead:

```clj
{...
 :deps {...
        athos/jise {:git/url "https://github.com/athos/JiSE.git"
                    :sha "<commit sha>"}
        ...}
 ...}
```

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
