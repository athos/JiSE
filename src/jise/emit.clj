(ns jise.emit
  (:require [jise.insns :as insns]
            [jise.type :as t])
  (:import [clojure.asm ClassVisitor ClassWriter Label MethodVisitor Opcodes Type]
           [clojure.lang Compiler DynamicClassLoader]))

(set! *warn-on-reflection* true)

(def ^:dynamic *env*
  {:continue-label nil
   :break-label nil
   :labels {}})

(defn access-value [flags]
  (cond-> 0
    (:static flags) (+ Opcodes/ACC_STATIC)
    (:public flags) (+ Opcodes/ACC_PUBLIC)
    (:protected flags) (+ Opcodes/ACC_PROTECTED)
    (:private flags) (+ Opcodes/ACC_PRIVATE)
    (:final flags) (+ Opcodes/ACC_FINAL)))

(defn emit-field [^ClassWriter cw {:keys [access name type value]}]
  (let [access (access-value access)
        desc (.getDescriptor ^Type type)]
   (doto (.visitField cw access (munge name) desc nil value)
     (.visitEnd))))

(defmulti emit-expr* (fn [mv expr] (:op expr)))
(defmethod emit-expr* :default [_ expr]
  (throw (ex-info (str "unknown expr found: " expr) {:expr expr})))

(defn emit-return [^MethodVisitor mv type]
  (.visitInsn mv (get insns/return-insns type Opcodes/ARETURN)))

(defn emit-expr [^MethodVisitor mv expr]
  (emit-expr* mv expr)
  (when (= (:context expr) :return)
    (emit-return mv (:type expr))))

(defn emit-method [^ClassWriter cw ctor? {:keys [name access return-type args body]}]
  (let [desc (->> (map :type args)
                  (into-array Type)
                  (Type/getMethodDescriptor return-type))
        mname (if ctor? "<init>" (munge name))
        mv (.visitMethod cw (access-value access) mname desc nil nil)]
    (doseq [arg args]
      (.visitParameter mv (:name arg) (access-value (:access arg))))
    (.visitCode mv)
    (when ctor?
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL
                        (.getInternalName ^Type t/OBJECT)
                        "<init>"
                        (Type/getMethodDescriptor t/VOID (into-array Type []))))
    (emit-expr mv body)
    (when (= return-type t/VOID)
      (emit-return mv t/VOID))
    (.visitMaxs mv 1 1)
    (.visitEnd mv)))

(defn emit-class [{:keys [name access parent interfaces ctors fields methods]}]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)]
    (.visit cw Opcodes/V1_5
            (access-value access)
            name
            nil
            (.getInternalName ^Type parent)
            (into-array String (map #(.getInternalName ^Type %) interfaces)))
    (doseq [field fields]
      (emit-field cw field))
    (doto (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 1 1)
      (.visitEnd))
    (doseq [ctor ctors]
      (emit-method cw true ctor))
    (doseq [method methods]
      (emit-method cw false method))
    (.visitEnd cw)
    (.toByteArray cw)))

(defmethod emit-expr* :do [mv {:keys [exprs]}]
  (doseq [expr exprs]
    (emit-expr mv expr)))

(defn drop-if-statement [^MethodVisitor mv context]
  (when (= context :statement)
    (let [insn (if (= (t/type-category type) 2)
                 Opcodes/POP2
                 Opcodes/POP)]
      (.visitInsn mv insn))))

(defmethod emit-expr* :null [^MethodVisitor mv {:keys [context]}]
  (when-not (= context :statement)
    (.visitInsn mv Opcodes/ACONST_NULL)))

(defmethod emit-expr* :literal [^MethodVisitor mv {:keys [type value context]}]
  (when-not (= context :statement)
    (if-let [insn (get-in insns/const-insns [type value])]
      (.visitInsn mv insn)
      (.visitLdcInsn mv value))))

(defn emit-load [^MethodVisitor mv type index]
  (.visitVarInsn mv (get insns/load-insns type Opcodes/ALOAD) index))

(defmethod emit-expr* :local [mv {:keys [type index context]}]
  (when-not (= context :statement)
    (emit-load mv type index)))

(defn emit-arithmetic [^MethodVisitor mv {:keys [type lhs rhs context]} op]
  (let [insn (get-in insns/arithmetic-insns [type op])]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv insn)
    (drop-if-statement mv context)))

(defmethod emit-expr* :add [mv expr]
  (emit-arithmetic mv expr :add))

(defmethod emit-expr* :sub [mv expr]
  (emit-arithmetic mv expr :sub))

(defmethod emit-expr* :mul [mv expr]
  (emit-arithmetic mv expr :mul))

(defmethod emit-expr* :div [mv expr]
  (emit-arithmetic mv expr :div))

(defmethod emit-expr* :rem [mv expr]
  (emit-arithmetic mv expr :rem))

(defmethod emit-expr* :conversion [^MethodVisitor mv {:keys [type src context]}]
  (emit-expr mv src)
  (case type
    (byte char short)
    (do (when-let [insn (get-in insns/conversion-insns [(:type src) 'int])]
          (.visitInsn mv insn))
        (.visitInsn mv (get-in insns/conversion-insns ['int type])))
    (do (.visitInsn mv (get-in insns/conversion-insns [(:type src) type]))
        (drop-if-statement mv context))))

(defn emit-store [^MethodVisitor mv {:keys [type index]}]
  (.visitVarInsn mv (get insns/store-insns type Opcodes/ASTORE) index))

(defmethod emit-expr* :let [mv {:keys [bindings body]}]
  (doseq [{:keys [init] :as b} bindings]
    (emit-expr mv init)
    (emit-store mv b))
  (emit-expr mv body))

(defn emit-dup [^MethodVisitor mv type]
  (let [insn (case type
               (long double) Opcodes/DUP2
               Opcodes/DUP)]
    (.visitInsn mv insn)))

(defn dup-unless-statement [mv context type]
  (when-not (= context :statement)
    (emit-dup mv type)))

(defmethod emit-expr* :assignment [^MethodVisitor mv {:keys [lhs rhs context]}]
  (emit-expr mv rhs)
  (dup-unless-statement mv context (:type rhs))
  (emit-store mv lhs))

(defmethod emit-expr* :increment [^MethodVisitor mv {:keys [target by context]}]
  (let [{:keys [type index]} target]
    (.visitIincInsn mv index by)
    (when-not (= context :statement)
      (emit-load mv type index))))

(defmethod emit-expr* :labeled [^MethodVisitor mv {:keys [label target kind]}]
  (let [break-label (Label.)]
    (binding [*env* (assoc-in *env* [:labels label] {:break-label break-label})]
      (emit-expr mv target))
    (.visitLabel mv break-label)))

(defn emit-conditional [^MethodVisitor mv cond label]
  (let [op (:op cond)]
    (case op
      (:eq :ne :lt :gt :le :ge)
      (let [{:keys [lhs rhs]} cond
            t (:type lhs)]
        (emit-expr mv lhs)
        (emit-expr mv rhs)
        (if-let [[insn1 insn2] (get-in insns/comparison-insns [t op])]
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

(defmethod emit-expr* :new [^MethodVisitor mv {:keys [type access arg-types args context]}]
  (let [method-type (Type/getMethodType t/VOID (into-array Type arg-types))
        iname (.getInternalName ^Type type)
        desc (.getDescriptor ^Type method-type)]
    (.visitTypeInsn mv Opcodes/NEW iname)
    (dup-unless-statement mv context type)
    (doseq [arg args]
      (emit-expr mv arg))
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL iname "<init>" desc false)))

(defmethod emit-expr* :field-access
  [^MethodVisitor mv {:keys [type name class target context]}]
  (when target
    (emit-expr mv target))
  (let [insn (if target Opcodes/GETFIELD Opcodes/GETSTATIC)
        owner (.getInternalName ^Type class)
        desc (.getDescriptor ^Type type)]
    (.visitFieldInsn mv insn owner name desc)
    (drop-if-statement mv context)))

(defmethod emit-expr* :field-update
  [^MethodVisitor mv {:keys [type name class target rhs context]}]
  (when target
    (emit-expr mv target))
  (emit-expr mv rhs)
  (when-not (= context :statement)
    (let [t (:type rhs)]
      (if target
        (let [insn (if (= (t/type-category t) 2) Opcodes/DUP_X2 Opcodes/DUP_X1)]
          (.visitInsn mv insn))
        (dup-unless-statement mv context t))))
  (let [insn (if target Opcodes/PUTFIELD Opcodes/PUTSTATIC)
        owner (.getInternalName ^Type class)
        desc (.getDescriptor ^Type type)]
    (.visitFieldInsn mv insn owner name desc)))

(defmethod emit-expr* :method-invocation
  [^MethodVisitor mv {:keys [type name access class arg-types target args context]}]
  (when target
    (emit-expr mv target))
  (doseq [arg args]
    (emit-expr mv arg))
  (let [method-type (Type/getMethodType ^Type type (into-array Type arg-types))
        insn (cond (:private access) Opcodes/INVOKESPECIAL
                   target Opcodes/INVOKEVIRTUAL
                   :else Opcodes/INVOKESTATIC)
        iname (.getInternalName ^Type class)
        desc (.getDescriptor method-type)]
    (.visitMethodInsn mv insn iname name desc false))
  (when-not (= type t/VOID)
    (drop-if-statement mv context)))

(def primitive-types
  {t/BOOLEAN Opcodes/T_BOOLEAN
   t/BYTE Opcodes/T_BYTE
   t/CHAR Opcodes/T_CHAR
   t/SHORT Opcodes/T_SHORT
   t/INT Opcodes/T_INT
   t/LONG Opcodes/T_LONG
   t/FLOAT Opcodes/T_FLOAT
   t/DOUBLE Opcodes/T_DOUBLE})

(defmethod emit-expr* :new-array [^MethodVisitor mv {:keys [type length context]}]
  (emit-expr mv length)
  (let [elem-type (t/element-type type)]
    (if (t/primitive-type? elem-type)
      (let [t (primitive-types elem-type)]
        (.visitIntInsn mv Opcodes/NEWARRAY t))
      (.visitTypeInsn mv Opcodes/ANEWARRAY (.getInternalName elem-type))))
  (drop-if-statement mv context))

(defmethod emit-expr* :array-access [^MethodVisitor mv {:keys [array index context]}]
  (emit-expr mv array)
  (emit-expr mv index)
  (let [elem-type (t/element-type (:type array))]
    (.visitInsn mv (get insns/aload-insns elem-type Opcodes/AALOAD))
    (drop-if-statement mv context)))

(defmethod emit-expr* :array-update [^MethodVisitor mv {:keys [array index expr context]}]
  (emit-expr mv array)
  (emit-expr mv index)
  (emit-expr mv expr)
  (dup-unless-statement mv context (:type expr))
  (let [elem-type (t/element-type (:type array))]
    (.visitInsn mv (get insns/astore-insns elem-type Opcodes/AASTORE))))
