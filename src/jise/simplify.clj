(ns jise.simplify
  (:require [jise.misc :as misc]
            [jise.type :as t]))

(declare simplify)

(defmulti simplify* (fn [cenv expr] (misc/symbol-without-jise-ns (first expr))))
(defmethod simplify* :default [cenv expr]
  (let [expanded (misc/macroexpand cenv expr)]
    (when-not (identical? expanded expr)
      (simplify cenv expanded))))

(defn simplify [cenv expr]
  (cond (or (boolean? expr) (char? expr) (string? expr)) expr
        (int? expr) (int expr)
        (float? expr) (double expr)
        (seq? expr) (try (simplify* cenv expr) (catch Exception _))
        (symbol? expr) (let [ns (namespace expr)
                             class (if ns
                                     (t/tag->type cenv (symbol ns) :throws-on-failure? false)
                                     (when (not (contains? (:locals cenv) expr))
                                       (:class-type cenv)))
                             caller (:class-type cenv)]
                         (when-let [field (and class (t/find-field cenv caller class (name expr)))]
                           (let [{:keys [access value]} field]
                             (when (and (:static access) (:final access) value)
                               value))))
        :else nil))

(defn- simplify-exprs [cenv exprs]
  (let [exprs' (map (partial simplify cenv) exprs)]
    (when-not (some nil? exprs')
      exprs')))

(defn- simplify-arithmetic [cenv [_ & args] op]
  (if-some [args' (simplify-exprs cenv args)]
    (let [res (apply op args')]
      (if (boolean? res)
        res
        (cond (some double? args') (double res)
              (some float? args') (float res)
              (some #(instance? Long %) args') (long res)
              :else (int res))))))

(defmethod simplify* '+ [cenv expr]
  (simplify-arithmetic cenv expr +))

(defmethod simplify* '- [cenv expr]
  (simplify-arithmetic cenv expr -))

(defmethod simplify* '* [cenv expr]
  (simplify-arithmetic cenv expr *))

(defmethod simplify* '/ [cenv expr]
  (let [div (fn [x y]
              (if (or (float? x) (float? y))
                (/ x y)
                (quot x y)))
        div* (fn [& args] (reduce div args))]
    (simplify-arithmetic cenv expr div*)))

(defmethod simplify* '% [cenv expr]
  (simplify-arithmetic cenv expr rem))

(defmethod simplify* '& [cenv expr]
  (simplify-arithmetic cenv expr bit-and))

(defmethod simplify* '| [cenv expr]
  (simplify-arithmetic cenv expr bit-or))

(defmethod simplify* 'xor [cenv expr]
  (simplify-arithmetic cenv expr bit-xor))

(defmethod simplify* '! [cenv expr]
  (simplify-arithmetic cenv expr bit-not))

(defmethod simplify* '== [cenv expr]
  (let [eql (fn [x y]
              (if (and (or (int? x) (float? x))
                       (or (int? y) (float? y)))
                 (== x y)
                 (= x y)))
        eql* (fn [& args] (reduce (fn [acc [x y]] (and acc (eql x y))) args))]
    (simplify-arithmetic cenv expr eql*)))

(defmethod simplify* '< [cenv expr]
  (simplify-arithmetic cenv expr <))

(defmethod simplify* '> [cenv expr]
  (simplify-arithmetic cenv expr >))

(defmethod simplify* '<= [cenv expr]
  (simplify-arithmetic cenv expr <=))

(defmethod simplify* '>= [cenv expr]
  (simplify-arithmetic cenv expr >=))

(defn- simplify-shift [cenv [_ expr1 expr2] op]
  (when-some [[expr1' expr2'] (simplify-exprs cenv [expr1 expr2])]
    (cond-> (op expr1' expr2)
      (instance? Integer expr1')
      int)))

(defmethod simplify* '<< [cenv expr]
  (simplify-shift cenv expr bit-shift-left))

(defmethod simplify* '>> [cenv expr]
  (simplify-shift cenv expr bit-shift-right))

(defmethod simplify* '>>> [cenv expr]
  (simplify-shift cenv expr unsigned-bit-shift-right))

(defn- simplify-cast [cenv [_ expr] op]
  (when-let [expr' (simplify cenv expr)]
    (op expr')))

(defmethod simplify* 'byte [cenv expr]
  (simplify-cast cenv expr byte))

(defmethod simplify* 'short [cenv expr]
  (simplify-cast cenv expr short))

(defmethod simplify* 'int [cenv expr]
  (simplify-cast cenv expr int))

(defmethod simplify* 'long [cenv expr]
  (simplify-cast cenv expr long))

(defmethod simplify* 'do [cenv [_ & exprs]]
  (when (= (count exprs) 1)
    (simplify cenv (first exprs))))

(defmethod simplify* 'if [cenv [_ test then else]]
  (when-some [test' (simplify cenv test)]
    (if test'
      (simplify cenv then)
      (simplify cenv else))))

(defmethod simplify* 'and [cenv [_ & args]]
  (when-some [args' (simplify-exprs cenv args)]
    (reduce #(and %1 %2) args')))

(defmethod simplify* 'or [cenv [_ & args]]
  (when-some [args' (simplify-exprs cenv args)]
    (reduce #(or %1 %2) args')))

(defmethod simplify* 'not [cenv [_ expr]]
  (when-some [expr' (simplify cenv expr)]
    (not expr')))

(defmethod simplify* 'str [cenv [_ & args]]
  (when-some [args' (simplify-exprs cenv args)]
    (apply str args')))
