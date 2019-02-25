(ns jise.emit
  (:require [jise.parse :as parse])
  (:import [clojure.asm ClassVisitor ClassWriter Label MethodVisitor Opcodes Type]
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
               (int short char byte boolean) Opcodes/IRETURN
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

(defmethod emit-expr* :literal [^MethodVisitor mv {:keys [type value]}]
  (let [consts {'int {-1 Opcodes/ICONST_M1, 0 Opcodes/ICONST_0
                      1 Opcodes/ICONST_1, 2 Opcodes/ICONST_2
                      3 Opcodes/ICONST_3,4 Opcodes/ICONST_4
                      5 Opcodes/ICONST_5}
                'long {0 Opcodes/LCONST_0, 1 Opcodes/LCONST_1}
                'float {0 Opcodes/FCONST_0, 1 Opcodes/FCONST_1
                        2 Opcodes/FCONST_2}
                'double {0 Opcodes/DCONST_0, 1 Opcodes/DCONST_1}
                'boolean {true Opcodes/ICONST_1, false Opcodes/ICONST_0}}]
    (if-let [insn (get-in consts [type value])]
      (.visitInsn mv insn)
      (.visitLdcInsn mv value))))

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

(defn emit-comparison-expr [mv expr]
  (emit-expr mv {:op :if :type 'int :test expr
                 :then (parse/parse-expr {} true)
                 :else (parse/parse-expr {} false)}))

(defmethod emit-expr* :eq [mv expr]
  (emit-comparison-expr mv expr))

(defmethod emit-expr* :ne [mv expr]
  (emit-comparison-expr mv expr))

(defmethod emit-expr* :lt [mv expr]
  (emit-comparison-expr mv expr))

(defmethod emit-expr* :gt [mv expr]
  (emit-comparison-expr mv expr))

(defmethod emit-expr* :le [mv expr]
  (emit-comparison-expr mv expr))

(defmethod emit-expr* :ge [mv expr]
  (emit-comparison-expr mv expr))

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

(def comparison-insns
  {'int {:eq [Opcodes/IF_ICMPNE], :ne [Opcodes/IF_ICMPEQ]
         :lt [Opcodes/IF_ICMPGE], :gt [Opcodes/IF_ICMPLE]
         :le [Opcodes/IF_ICMPGT], :ge [Opcodes/IF_ICMPLT]}
   'long {:eq [Opcodes/LCMP Opcodes/IFNE], :ne [Opcodes/LCMP Opcodes/IFEQ]
          :lt [Opcodes/LCMP Opcodes/IFGE], :gt [Opcodes/LCMP Opcodes/IFLE]
          :le [Opcodes/LCMP Opcodes/IFGT], :ge [Opcodes/LCMP Opcodes/IFLT]}
   'float {:eq [Opcodes/FCMPL Opcodes/IFNE], :ne [Opcodes/FCMPL Opcodes/IFEQ]
           :lt [Opcodes/FCMPL Opcodes/IFGE], :gt [Opcodes/FCMPL Opcodes/IFLE]
           :le [Opcodes/FCMPL Opcodes/IFGT], :ge [Opcodes/FCMPL Opcodes/IFLT]}
   'double {:eq [Opcodes/DCMPL Opcodes/IFNE], :ne [Opcodes/DCMPL Opcodes/IFEQ]
            :lt [Opcodes/DCMPL Opcodes/IFGE], :gt [Opcodes/DCMPL Opcodes/IFLE]
            :le [Opcodes/DCMPL Opcodes/IFGT], :ge [Opcodes/DCMPL Opcodes/IFLT]}})

(defmethod emit-expr* :if [^MethodVisitor mv {:keys [test then else]}]
  (let [op (:op test)
        else-label (Label.)
        end-label (Label.)]
    (case op
      (:eq :ne :lt :gt :le :lg)
      (let [{:keys [lhs rhs]} test
            t (:type lhs)]
        (emit-expr mv lhs)
        (emit-expr mv rhs)
        (if-let [[insn1 insn2] (get-in comparison-insns [t op])]
          (if insn2
            (do (.visitInsn mv insn1)
                (.visitJumpInsn mv insn2 else-label))
            (.visitJumpInsn mv insn1 else-label))
          (let [insn (case op
                       :eq Opcodes/IF_ACMPNE
                       :ne Opcodes/IF_ACMPEQ)]
            (.visitJumpInsn mv insn else-label))))
      (let [msg (str "not supported conditional: " op)]
        (throw (ex-info msg {:op op}))))
    (emit-expr mv then)
    (.visitJumpInsn mv Opcodes/GOTO end-label)
    (.visitLabel mv else-label)
    (emit-expr mv else)
    (.visitLabel mv end-label)))
