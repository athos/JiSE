(ns jise.extension.tagbody
  (:require [jise.emit :as emit]
            [jise.parse :as parse])
  (:import [clojure.asm Label MethodVisitor Opcodes]))

(defn parse-tagbody-body [body]
  (loop [body body
         tag nil
         block []
         blocks []]
    (if (empty? body)
      (conj blocks [tag block])
      (let [[expr & more] body]
        (if (keyword? expr)
          (recur more expr [] (conj blocks [tag block]))
          (recur more tag (conj block expr) blocks))))))

(defmethod parse/parse-expr* `tagbody* [cenv [_ & body]]
  (let [blocks (parse-tagbody-body body)
        [last-tag last-body] (last blocks)
        cenv' (parse/inherit-context cenv cenv :return? false)
        last' (parse/parse-expr cenv' `(do ~@last-body))]
    (-> {:op ::tagbody
         :type (:type last')
         :blocks (conj (mapv (fn [[tag body]]
                               (let [cenv' (parse/with-context cenv :statement)]
                                 [tag (parse/parse-expr cenv'`(do ~@body))]))
                             (butlast blocks))
                       [last-tag last'])}
        (parse/inherit-context cenv))))

(defmethod parse/parse-expr* `go* [cenv [_ tag]]
  (-> {:op ::go :tag tag}
      (parse/inherit-context (parse/with-context cenv :statement))))

(defmacro tagbody [& body]
  `(tagbody* ~@body))

(defmacro go [tag]
  `(go* ~tag))

(defmethod emit/emit-expr* ::tagbody [{:keys [^MethodVisitor mv] :as emitter} {:keys [blocks context]}]
  (let [tag->label (zipmap (map first blocks) (repeatedly #(Label.)))
        emitter' (assoc emitter ::tag->label tag->label)]
    (doseq [[tag body] blocks]
      (when-let [label (some-> tag tag->label)]
        (.visitLabel mv label))
      (emit/emit-expr emitter' body))))

(defmethod emit/emit-expr* ::go [{:keys [^MethodVisitor mv ::tag->label]} {:keys [tag]}]
  (.visitJumpInsn mv Opcodes/GOTO (get tag->label tag)))

(comment

  (require '[jise.core :refer [defclass]])
  (alias 'tb 'jise.extension.tagbody)

  ^:public
  (defclass Hello
    ^:public ^:static
    (defm hello []
      (let [n 3]
        (tb/tagbody
         :point-a
         (.print System/out "h")
         (tb/go :point-c)
         :point-b
         (.println System/out "world")
         (if (> (dec! n) 0)
           (tb/go :point-a)
           (return))
         :point-c
         (.print System/out "el")
         :point-d
         (.print System/out "lo ")
         (tb/go :point-b)))))

  (Hello/hello)

  )
