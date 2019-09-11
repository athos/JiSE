(ns jise.error
  (:require [clojure.string :as str]
            [jise.type :as t]))

(def ^:dynamic *line* nil)
(def ^:dynamic *column* nil)

(defmacro with-line&column-of [x & body]
  `(let [{line# :line column# :column} (meta ~x)]
     (if (and line# column#)
       (binding [*line* line# *column* column#]
         ~@body)
       (do ~@body))))

(defn stringify-type [t]
  (if (nil? t)
    "<null>"
    (-> (t/type->tag t) str (str/replace #"^java\.lang\.([^.]+)$" "$1"))))

(defmacro error [msg & [data]]
  `(let [msg# (str "Error: " ~msg " (" *file* \: *line* \: *column* ")")
         data# (merge {:line *line* :column *column*} ~data)]
     (throw (ex-info msg# data#))))

(defmacro error-on-reserved-word [keyword]
  `(error ~(str keyword " is a reserved word, but cannot be used now")))

(defn error-message-on-incompatible-types [expected actual]
  (format "incompatible types: %s cannot be converted to %s"
          (stringify-type actual)
          (stringify-type expected)))

(defmacro error-on-incompatible-types [expected actual]
  `(error (error-message-on-incompatible-types ~expected ~actual)))

(defmacro error-on-bad-operand-type [op-name t]
  `(error (format "bad operand type %s for unary operator '%s'"
                  (stringify-type ~t) ~op-name)))

(defmacro error-on-bad-operand-types [op-name t1 t2]
  `(error (str "bad operand types for binary operator '" ~op-name "'\n"
               "  first type: " (stringify-type ~t1) "\n"
               "  second type: " (stringify-type ~t2))))

(defn error-on-missing-arguments [op-name num varargs?]
  (error (str (when varargs?
                "at least ")
              num (if (= num 1) " argument " " arguments ")
              "required for operator '" op-name "'")))

(defn handle-ctor-error [class arg-types e]
  (if-let [cause (:cause (ex-data e))]
    (let [class-name (stringify-type class)]
      (-> (case cause
            :no-such-target
            (format "cannot find symbol: method %s(%s)" class-name
                    (str/join "," (map stringify-type arg-types)))
            :args-length-mismatch
            (str "constructor " class-name " in class " class-name
                 " cannot be applied to given types")
            :arg-type-mismatch
            (format "no suitable constructor found for %s(%s)"
                    class-name (str/join "," (map stringify-type arg-types)))
            (ex-message e))
          (error (dissoc (ex-data e) :cause))))
    (throw e)))

(defn- param-types-string [param-types]
  (if (seq param-types)
    (str/join \, (map stringify-type param-types))
    "no arguments"))

(defn- signature-string [name param-types]
  (format "%s(%s)" name (param-types-string param-types)))

(defn- first-mismatched-arg-type [cenv arg-types method]
  (try
    (if (:varargs (:access method))
      (t/variable-arity-invocation-conversion cenv arg-types method)
      (t/loose-invocation-conversion cenv arg-types method))
    (catch Exception e
      (let [{:keys [param-type arg-type]} (ex-data e)]
        [param-type arg-type]))))

(defn handle-method-error [cenv class name arg-types e]
  (let [{:keys [cause] :as ed} (ex-data e)
        class-name (stringify-type class)]
    (if cause
      (-> (case cause
            :no-such-target
            (str "cannot find symbol\n"
                 "  symbol: method " (signature-string name arg-types) "\n"
                 "  location: class " class-name)
            :args-length-mismatch
            (let [{[m :as ms] :alternatives} ed
                  reason "actual and formal argument lists differ in length"]
              (if (= (count ms) 1)
                (str "method " name " in class " class-name
                     " cannot be applied to given types\n"
                     "  required: " (param-types-string (:param-types m)) "\n"
                     "  found: " (param-types-string arg-types) "\n"
                     "  reason: " reason)
                (str "no suitable method found for " (signature-string name arg-types) "\n"
                     (->> (for [{:keys [access param-types]} ms]
                            (format "  method %s is not applicable\n    (%s)"
                                    (cond->> (signature-string name param-types)
                                      (:static access) (str class-name \.))
                                    reason))
                          (str/join \newline)))))
            :arg-type-mismatch
            (let [{[m :as ms] :alternatives} ed]
              (if (= (count ms) 1)
                (let [[pt at] (first-mismatched-arg-type cenv arg-types m)]
                  (error-message-on-incompatible-types pt at))
                (str "no suitable method found for "
                     (signature-string name arg-types) "\n"
                     (->> (for [{:keys [access param-types] :as m} ms
                                :let [[pt at] (first-mismatched-arg-type cenv arg-types m)]]
                            (format "  method %s is not applicable\n    (argument mismatch; %s)"
                                    (cond->> (signature-string name param-types)
                                      (:static access) (str class-name \.))
                                    (error-message-on-incompatible-types pt at)))
                          (str/join \newline)))))
            :ambiguous-invocation
            (let [[m1 m2] (:alternatives ed)]
              (str "reference to " name " is ambiguous\n"
                   "  both method " (signature-string name (:param-types m1))
                   " in " (stringify-type (:class m1))
                   " and method " (signature-string name (:param-types m2))
                   " in " (stringify-type (:class m2))
                   " match"))
            (ex-message e))
          (error (dissoc ed :cause)))
      (throw e))))
