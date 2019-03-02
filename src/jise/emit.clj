(ns jise.emit
  (:require [jise.insns :as insns]
            [jise.parse :as parse])
  (:import [clojure.asm ClassVisitor ClassWriter Label MethodVisitor Opcodes Type]
           [clojure.lang Compiler DynamicClassLoader]))

(set! *warn-on-reflection* true)

(def ^:dynamic *env*
  {:continue-label nil
   :break-label nil
   :labels {}})

(defn ^Type ->type [x]
  (cond (class? x) (Type/getType ^Class x)
        (parse/array-type? x) (let [^Type elem-type (->type (parse/element-type x))]
                                (Type/getType (str \[ (.getDescriptor elem-type))))
        :else (insns/primitive-types x)))

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

(defn emit-method [^ClassWriter cw {:keys [name access return-type args body]}]
  (let [desc (->> (map (comp ->type :type) args)
                  (into-array Type)
                  (Type/getMethodDescriptor (->type return-type)))
        mv (.visitMethod cw (access-value access) (munge name) desc nil nil)]
    (doseq [arg args]
      (.visitParameter mv (:name arg) (access-value (:access arg))))
    (.visitCode mv)
    (emit-expr mv body)
    (when (= return-type 'void)
      (emit-return mv 'void))
    (.visitMaxs mv 1 1)
    (.visitEnd mv)))

(defn emit-class [{:keys [name access parent interfaces fields methods]}]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)]
    (.visit cw Opcodes/V1_5
            (access-value access)
            name
            nil
            (.getInternalName (->type parent))
            (into-array String (map #(.getInternalName (->type %)) interfaces)))
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
  (let [insn (get-in insns/arithmetic-insns [op type])]
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

(defmethod emit-expr* :field-access
  [^MethodVisitor mv {:keys [type name class target context]}]
  (emit-expr mv target)
  (let [owner (.getInternalName (->type class))
        desc (.getDescriptor (->type type))]
    (.visitFieldInsn mv Opcodes/GETFIELD owner name desc)))

(defmethod emit-expr* :new-array [^MethodVisitor mv {:keys [type length context]}]
  (emit-expr mv length)
  (let [elem-type (parse/element-type type)]
    (if (parse/primitive-types elem-type)
      (let [insn (case elem-type
                   int Opcodes/T_INT
                   short Opcodes/T_SHORT
                   long Opcodes/T_LONG
                   float Opcodes/T_FLOAT
                   double Opcodes/T_DOUBLE
                   char Opcodes/T_CHAR
                   byte Opcodes/T_BYTE)]
        (.visitIntInsn mv Opcodes/NEWARRAY insn))
      (.visitTypeInsn mv Opcodes/ANEWARRAY (.getInternalName (->type elem-type)))))
  (drop-if-statement mv context))

(defmethod emit-expr* :array-access [^MethodVisitor mv {:keys [array index context]}]
  (emit-expr mv array)
  (emit-expr mv index)
  (let [elem-type (parse/element-type (:type array))]
    (.visitInsn mv (get insns/aload-insns elem-type Opcodes/AALOAD))
    (drop-if-statement mv context)))

(defmethod emit-expr* :array-update [^MethodVisitor mv {:keys [array index expr context]}]
  (emit-expr mv array)
  (emit-expr mv index)
  (emit-expr mv expr)
  (dup-unless-statement mv context (:type expr))
  (let [elem-type (parse/element-type (:type array))]
    (.visitInsn mv (get insns/astore-insns elem-type Opcodes/AASTORE))))
