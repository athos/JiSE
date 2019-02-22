(ns jise.core
  (:import [clojure.asm ClassVisitor ClassWriter MethodVisitor Opcodes Type]
           [clojure.lang Compiler]))

(defn parse-class-body [body]
  (loop [decls body
         ret {:fields []
              :methods []}]
    (if (empty? decls)
      ret
      (let [[decl & decls] decls]
        (if (seq? decl)
          (case (first decl)
            def (recur decls (update ret :fields conj decl))
            defm (recur decls (update ret :methods conj decl))
            do (recur (concat (rest decl) decls) ret)
            (if-let [v (resolve (first decl))]
              (if (:macro (meta v))
                (recur (cons (macroexpand decl) decls) ret)
                (let [msg (str "unknown type of declaration found: " (first decl))]
                  (throw (ex-info msg {:decl decl}))))
              (let [msg (str "unknown type of declaration found: " (first decl))]
                (throw (ex-info msg {:decl decl})))))
          (recur decls ret))))))

(defn modifiers-of [[_ name :as form]]
  (merge (meta form) (meta name)))

(def primitive-types
  {'int Type/INT_TYPE
   'short Type/SHORT_TYPE
   'long Type/LONG_TYPE
   'float Type/FLOAT_TYPE
   'double Type/DOUBLE_TYPE
   'char Type/CHAR_TYPE
   'boolean Type/BOOLEAN_TYPE
   'void Type/VOID_TYPE})

(defn tag->type [tag & {:keys [default]}]
  (or (get primitive-types tag)
      (when-let [c (and (symbol? tag) (resolve tag))]
        (when (class? c)
          (Type/getType c)))
      default
      (Type/getType Object)))

(defn access-flags [modifiers]
  (cond-> 0
    (:static modifiers) (+ Opcodes/ACC_STATIC)
    (:public modifiers) (+ Opcodes/ACC_PUBLIC)
    (:protected modifiers) (+ Opcodes/ACC_PROTECTED)
    (:private modifiers) (+ Opcodes/ACC_PRIVATE)
    (:final modifiers) (+ Opcodes/ACC_FINAL)))

(defn parse-modifiers [{:keys [tag] :as modifiers}]
  {:type (tag->type tag)
   :access (access-flags modifiers)})

(defn emit-field [^ClassWriter cw [_ fname value :as field]]
  (let [modifiers (modifiers-of field)
        {:keys [access ^Type type]} (parse-modifiers modifiers)]
    (doto (.visitField cw access (str fname) (.getDescriptor type) nil value)
      (.visitEnd))))

(defn emit-method [^ClassWriter cw [_ mname args :as method]]
  (let [modifiers (modifiers-of method)
        {:keys [access ^Type type]} (parse-modifiers modifiers)
        desc (->> (map (comp tag->type :tag meta) args)
                  (into-array Type)
                  (Type/getMethodDescriptor type))
        mv (.visitMethod cw access (str mname) desc nil nil)]
    (doseq [arg args]
      (.visitParameter mv (str arg) (access-flags (meta arg))))
    (doto mv
      (.visitCode)
      (.visitInsn Opcodes/ICONST_0)
      (.visitInsn Opcodes/IRETURN)
      (.visitMaxs 1 1)
      (.visitEnd))))

(defn emit [cname modifiers fields methods]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)]
    (.visit cw Opcodes/V1_5
            (access-flags modifiers)
            cname
            nil
            "java/lang/Object"
            nil)
    (doseq [field fields]
      (emit-field cw field))
    (doto (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 1 1)
      (.visitEnd))
    (doseq [method methods]
      (emit-method cw method))
    (.visitEnd cw)
    (.toByteArray cw)))

(defmacro defclass [cname & body]
  (let [cname' (str cname)
        modifiers (modifiers-of &form)
        {:keys [fields methods]} (parse-class-body body)
        bytecode (emit cname' modifiers fields methods)]
    (when *compile-files*
      (Compiler/writeClassFile cname' bytecode))
    (.defineClass @Compiler/LOADER cname' bytecode nil)
    `(do (import '~cname)
         ~cname)))

(comment

 (defclass C
   ^:public ^String
   (def x nil)
   ^:public ^int
   (defm m []))

 )
