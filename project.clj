(defproject jise "0.1.0-SNAPSHOT"
  :description "JiSE: Java in S-Expression"
  :url "https://github.com/athos/JiSE"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :repl-options {:init-ns jise.core}
  :profiles {:dev {:source-paths ["examples"]}})
