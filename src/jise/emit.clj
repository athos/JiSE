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

(defmulti emit-expr* (fn [mv expr] (:op expr)))
(defmethod emit-expr* :default [_ expr]
  (throw (ex-info (str "unknown expr found: " expr) {:expr expr})))

(defn emit-expr [^MethodVisitor mv expr]
  (emit-expr* mv expr))

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
    (emit-expr mv body)
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

(defmethod emit-expr* :do [mv {:keys [exprs]}]
  (doseq [expr exprs]
    (emit-expr mv expr)))

(defmethod emit-expr* :null [^MethodVisitor mv _]
  (.visitInsn mv Opcodes/ACONST_NULL))

(defmethod emit-expr* :literal [^MethodVisitor mv {:keys [value]}]
  (.visitLdcInsn mv value))

(defmethod emit-expr* :local [^MethodVisitor mv {:keys [type index]}]
  (let [insn (case type
               (int short byte char) Opcodes/ILOAD
               long Opcodes/LLOAD
               float Opcodes/FLOAD
               double Opcodes/DLOAD
               Opcodes/ARETURN)]
    (.visitVarInsn mv insn index)))

(defmethod emit-expr* :add [^MethodVisitor mv {:keys [type lhs rhs]}]
  (let [insn (case type
               int Opcodes/IADD
               long Opcodes/LADD
               float Opcodes/FADD
               double Opcodes/DADD)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)))

(defmethod emit-expr* :sub [^MethodVisitor mv {:keys [type lhs rhs]}]
  (let [insn (case type
               int Opcodes/ISUB
               long Opcodes/LSUB
               float Opcodes/FSUB
               double Opcodes/DSUB)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)))

(defmethod emit-expr* :mul [^MethodVisitor mv {:keys [type lhs rhs]}]
  (let [insn (case type
               int Opcodes/IMUL
               long Opcodes/LMUL
               float Opcodes/FMUL
               double Opcodes/DMUL)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)))

(defmethod emit-expr* :div [^MethodVisitor mv {:keys [type lhs rhs]}]
  (let [insn (case type
               int Opcodes/IDIV
               long Opcodes/LDIV
               float Opcodes/FDIV
               double Opcodes/DDIV)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)))

(defmethod emit-expr* :conversion [^MethodVisitor mv {:keys [type src]}]
  (emit-expr mv src)
  (case type
    (byte char short)
    (do (when-let [insn (case (:type src)
                          long Opcodes/L2I
                          float Opcodes/F2I
                          double Opcodes/D2I)]
          (.visitInsn mv insn))
        (let [insn (case type
                     byte Opcodes/I2B
                     char Opcodes/I2C
                     short Opcodes/I2S)]
          (.visitInsn mv insn)))
    (let [insn (case [(:type src) type]
                 [int    long  ] Opcodes/I2L
                 [int    float ] Opcodes/I2F
                 [int    double] Opcodes/I2D
                 [long   int   ] Opcodes/L2I
                 [long   float ] Opcodes/L2F
                 [long   double] Opcodes/L2D
                 [float  int   ] Opcodes/F2I
                 [float  long  ] Opcodes/F2L
                 [float  double] Opcodes/F2D
                 [double int   ] Opcodes/D2I
                 [double long  ] Opcodes/D2L
                 [double float ] Opcodes/D2F)]
      (.visitInsn mv insn))))

(defn emit-store [^MethodVisitor mv {:keys [type index]}]
  (let [insn (case type
               int Opcodes/ISTORE
               long Opcodes/LSTORE
               float Opcodes/FSTORE
               double Opcodes/DSTORE
               Opcodes/ASTORE)]
    (.visitVarInsn mv insn index)))

(defmethod emit-expr* :let [mv {:keys [bindings body]}]
  (doseq [{:keys [init] :as b} bindings]
    (emit-expr mv init)
    (emit-store mv b))
  (emit-expr mv body))

(defmethod emit-expr* :assignment [mv {:keys [lhs rhs]}]
  (emit-expr mv rhs)
  (emit-store mv lhs))
