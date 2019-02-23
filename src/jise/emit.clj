(ns jise.emit
  (:import [clojure.asm ClassVisitor ClassWriter MethodVisitor Opcodes Type]
           [clojure.lang Compiler DynamicClassLoader]))

(set! *warn-on-reflection* true)

(def primitive-types
  {'int Type/INT_TYPE
   'short Type/SHORT_TYPE
   'long Type/LONG_TYPE
   'float Type/FLOAT_TYPE
   'double Type/DOUBLE_TYPE
   'char Type/CHAR_TYPE
   'boolean Type/BOOLEAN_TYPE
   'void Type/VOID_TYPE})

(defn ^Type ->type [x]
  (if (class? x)
    (Type/getType ^Class x)
    (primitive-types x)))

(def coerce-fns
  {'int int
   'short short
   'long long
   'byte byte})

(defn access-value [flags]
  (cond-> 0
    (:static flags) (+ Opcodes/ACC_STATIC)
    (:public flags) (+ Opcodes/ACC_PUBLIC)
    (:protected flags) (+ Opcodes/ACC_PROTECTED)
    (:private flags) (+ Opcodes/ACC_PRIVATE)
    (:final flags) (+ Opcodes/ACC_FINAL)))

(defn emit-field [^ClassWriter cw {:keys [access name type value]}]
  (let [access (access-value access)
        desc (.getDescriptor (->type type))]
   (doto (.visitField cw access name desc nil value)
     (.visitEnd))))

(declare emit-exprs)

(defn emit-expr [^MethodVisitor mv expr tail?]
  (if (map? expr)
    (case (:op expr)
      :literal (let [coerce (coerce-fns (:type expr) identity)]
                 (.visitLdcInsn mv (coerce (:value expr))))
      (throw (ex-info (str "unknown expr found: " expr) {:expr expr})))
    (emit-exprs mv expr))
  (when-not tail?
    (.visitInsn mv Opcodes/POP)))

(defn emit-exprs [^MethodVisitor mv exprs]
  (loop [[expr & exprs] exprs]
    (when expr
      (emit-expr mv expr (not (seq exprs)))
      (recur exprs))))

(defn emit-return [^MethodVisitor mv type]
  (let [insn (case type
               (int short char byte) Opcodes/IRETURN
               long Opcodes/LRETURN
               float Opcodes/FRETURN
               double Opcodes/DRETURN
               void Opcodes/RETURN
               Opcodes/ARETURN)]
    (.visitInsn mv insn)))

(defn emit-method [^ClassWriter cw {:keys [name access return-type args body]}]
  (let [desc (->> (map (comp ->type :type) args)
                  (into-array Type)
                  (Type/getMethodDescriptor (->type return-type)))
        mv (.visitMethod cw (access-value access) name desc nil nil)]
    (doseq [arg args]
      (.visitParameter mv (:name arg) (access-value (:access arg))))
    (.visitCode mv)
    (emit-exprs mv body)
    (doto mv
      (emit-return return-type)
      (.visitMaxs 1 1)
      (.visitEnd))))

(defn emit-class [{:keys [name access fields methods]}]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)]
    (.visit cw Opcodes/V1_5
            (access-value access)
            name
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
