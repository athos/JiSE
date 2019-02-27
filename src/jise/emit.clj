(ns jise.emit
  (:require [jise.parse :as parse])
  (:import [clojure.asm ClassVisitor ClassWriter Label MethodVisitor Opcodes Type]
           [clojure.lang Compiler DynamicClassLoader]))

(set! *warn-on-reflection* true)

(def ^:dynamic *env*
  {:continue-label nil
   :break-label nil
   :labels {}})

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

(defn emit-return [^MethodVisitor mv type]
  (let [insn (case type
               (int short char byte boolean) Opcodes/IRETURN
               long Opcodes/LRETURN
               float Opcodes/FRETURN
               double Opcodes/DRETURN
               void Opcodes/RETURN
               Opcodes/ARETURN)]
    (.visitInsn mv insn)))

(defn emit-expr [^MethodVisitor mv expr]
  (emit-expr* mv expr)
  (when (= (:context expr) :return)
    (emit-return mv (:type expr))))

(defn emit-method [^ClassWriter cw {:keys [name access return-type args body]}]
  (let [desc (->> (map (comp ->type :type) args)
                  (into-array Type)
                  (Type/getMethodDescriptor (->type return-type)))
        mv (.visitMethod cw (access-value access) name desc nil nil)]
    (doseq [arg args]
      (.visitParameter mv (:name arg) (access-value (:access arg))))
    (.visitCode mv)
    (emit-expr mv body)
    (.visitMaxs mv 1 1)
    (.visitEnd mv)))

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

(defn drop-if-statement [^MethodVisitor mv context]
  (when (= context :statement)
    (let [insn (case type
                 (long double) Opcodes/POP2
                 Opcodes/POP)]
      (.visitInsn mv insn))))

(defmethod emit-expr* :null [^MethodVisitor mv {:keys [context]}]
  (when-not (= context :statement)
    (.visitInsn mv Opcodes/ACONST_NULL)))

(defmethod emit-expr* :literal [^MethodVisitor mv {:keys [type value context]}]
  (let [consts {'int {-1 Opcodes/ICONST_M1, 0 Opcodes/ICONST_0
                      1 Opcodes/ICONST_1, 2 Opcodes/ICONST_2
                      3 Opcodes/ICONST_3,4 Opcodes/ICONST_4
                      5 Opcodes/ICONST_5}
                'long {0 Opcodes/LCONST_0, 1 Opcodes/LCONST_1}
                'float {0 Opcodes/FCONST_0, 1 Opcodes/FCONST_1
                        2 Opcodes/FCONST_2}
                'double {0 Opcodes/DCONST_0, 1 Opcodes/DCONST_1}
                'boolean {true Opcodes/ICONST_1, false Opcodes/ICONST_0}}]
    (when-not (= context :statement)
      (if-let [insn (get-in consts [type value])]
        (.visitInsn mv insn)
        (.visitLdcInsn mv value)))))

(defmethod emit-expr* :local [^MethodVisitor mv {:keys [type index context]}]
  (let [insn (case type
               (int short byte char) Opcodes/ILOAD
               long Opcodes/LLOAD
               float Opcodes/FLOAD
               double Opcodes/DLOAD
               Opcodes/ALOAD)]
    (when-not (= context :statement)
      (.visitVarInsn mv insn index))))

(defmethod emit-expr* :add [^MethodVisitor mv {:keys [type lhs rhs context]}]
  (let [insn (case type
               int Opcodes/IADD
               long Opcodes/LADD
               float Opcodes/FADD
               double Opcodes/DADD)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)
    (drop-if-statement mv context)))

(defmethod emit-expr* :sub [^MethodVisitor mv {:keys [type lhs rhs context]}]
  (let [insn (case type
               int Opcodes/ISUB
               long Opcodes/LSUB
               float Opcodes/FSUB
               double Opcodes/DSUB)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)
    (drop-if-statement mv context)))

(defmethod emit-expr* :mul [^MethodVisitor mv {:keys [type lhs rhs context]}]
  (let [insn (case type
               int Opcodes/IMUL
               long Opcodes/LMUL
               float Opcodes/FMUL
               double Opcodes/DMUL)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)
    (drop-if-statement mv context)))

(defmethod emit-expr* :div [^MethodVisitor mv {:keys [type lhs rhs context]}]
  (let [insn (case type
               int Opcodes/IDIV
               long Opcodes/LDIV
               float Opcodes/FDIV
               double Opcodes/DDIV)]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)
    (drop-if-statement mv context)))

(defmethod emit-expr* :conversion [^MethodVisitor mv {:keys [type src context]}]
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
          (.visitInsn mv insn)
          (drop-if-statement mv context)))
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
      (.visitInsn mv insn)
      (drop-if-statement mv context))))

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

(defmethod emit-expr* :assignment [^MethodVisitor mv {:keys [lhs rhs context]}]
  (emit-expr mv rhs)
  (when-not (= context :statement)
    (let [insn (case (:type rhs)
                 (long double) Opcodes/DUP2
                 Opcodes/DUP)]
      (.visitInsn mv insn)))
  (emit-store mv lhs))

(defmethod emit-expr* :increment [^MethodVisitor mv {:keys [target by]}]
  (.visitIincInsn mv (:index target) by))

(defmethod emit-expr* :labeled [^MethodVisitor mv {:keys [label target kind]}]
  (let [break-label (Label.)]
    (binding [*env* (assoc-in *env* [:labels label] {:break-label break-label})]
      (emit-expr mv target))
    (.visitLabel mv break-label)))

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

(defn emit-conditional [^MethodVisitor mv cond label]
  (let [op (:op cond)]
    (case op
      (:eq :ne :lt :gt :le :ge)
      (let [{:keys [lhs rhs]} cond
            t (:type lhs)]
        (emit-expr mv lhs)
        (emit-expr mv rhs)
        (if-let [[insn1 insn2] (get-in comparison-insns [t op])]
          (if insn2
            (do (.visitInsn mv insn1)
                (.visitJumpInsn mv insn2 label))
            (.visitJumpInsn mv insn1 label))
          (let [insn (case op
                       :eq Opcodes/IF_ACMPNE
                       :ne Opcodes/IF_ACMPEQ)]
            (.visitJumpInsn mv insn label))))
      (let [msg (str "not supported conditional: " op)]
        (throw (ex-info msg {:op op}))))))

(defmethod emit-expr* :if [^MethodVisitor mv {:keys [test then else]}]
  (let [end-label (Label.)
        else-label (if else (Label.) end-label)]
    (emit-conditional mv test else-label)
    (emit-expr mv then)
    (when else
      (.visitJumpInsn mv Opcodes/GOTO end-label)
      (.visitLabel mv else-label)
      (emit-expr mv else))
    (.visitLabel mv end-label)))

(defn with-labels [label-name continue-label break-label f]
  (let [env (-> *env*
                (assoc :continue-label continue-label :break-label break-label)
                (cond->
                  label-name
                  (assoc-in [:labels label-name :continue-label] continue-label)))]
    (binding [*env* env]
      (f))))

(defmethod emit-expr* :while [^MethodVisitor mv {:keys [cond body label context]}]
  (let [start-label (Label.)
        end-label (Label.)]
    (with-labels label start-label end-label
      (fn []
        (.visitLabel mv start-label)
        (emit-conditional mv cond end-label)
        (emit-expr mv body)
        (.visitJumpInsn mv Opcodes/GOTO start-label)
        (.visitLabel mv end-label)))
    (when-not (= context :statement)
      (.visitInsn mv Opcodes/ACONST_NULL))))

(defmethod emit-expr* :for [^MethodVisitor mv {:keys [cond step body label context]}]
  (let [start-label (Label.)
        continue-label (Label.)
        end-label (Label.)]
    (with-labels label continue-label end-label
      (fn []
        (.visitLabel mv start-label)
        (emit-conditional mv cond end-label)
        (emit-expr mv body)
        (.visitLabel mv continue-label)
        (emit-expr mv step)
        (.visitJumpInsn mv Opcodes/GOTO start-label)
        (.visitLabel mv end-label)))
    (when-not (= context :statement)
      (.visitInsn mv Opcodes/ACONST_NULL))))

(defmethod emit-expr* :continue [^MethodVisitor mv {:keys [label]}]
  (let [^Label label (if label
                       (get-in *env* [:labels label :continue-label])
                       (:continue-label *env*))]
    (.visitJumpInsn mv Opcodes/GOTO label)))

(defmethod emit-expr* :break [^MethodVisitor mv {:keys [label]}]
  (let [^Label label (if label
                       (get-in *env* [:labels label :break-label])
                       (:break-label *env*))]
    (.visitJumpInsn mv Opcodes/GOTO label)))
