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
    (.visit cw Opcodes/V1_8
            (+ (access-value access) Opcodes/ACC_SUPER)
            name
            nil
            (.getInternalName ^Type parent)
            (into-array String (map #(.getInternalName ^Type %) interfaces)))
    (doseq [field fields]
      (emit-field cw field))
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
    (let [opcode (if (= (t/type-category type) 2)
                 Opcodes/POP2
                 Opcodes/POP)]
      (.visitInsn mv opcode))))

(defmethod emit-expr* :null [^MethodVisitor mv {:keys [context]}]
  (when-not (= context :statement)
    (.visitInsn mv Opcodes/ACONST_NULL)))

(defn primitive-type [type]
  (if (#{t/BYTE t/CHAR t/SHORT} type) t/INT type))

(defmethod emit-expr* :literal [^MethodVisitor mv {:keys [type value context]}]
  (when-not (= context :statement)
    (if-let [opcode (get-in insns/const-insns [(primitive-type type) value])]
      (.visitInsn mv opcode)
      (cond (and (#{t/BYTE t/SHORT t/CHAR t/INT} type)
                 (<= Byte/MIN_VALUE (long value) Byte/MAX_VALUE))
            (.visitIntInsn mv Opcodes/BIPUSH (int value))

            (and (#{t/SHORT t/INT} type)
                 (<= Short/MIN_VALUE (long value) Short/MAX_VALUE))
            (.visitIntInsn mv Opcodes/SIPUSH (int value))

            :else (let [v (condp = type
                            t/INT (int value)
                            t/FLOAT (float value)
                            value)]
                    (.visitLdcInsn mv v))))))

(defn emit-load [^MethodVisitor mv type index]
  (.visitVarInsn mv (get insns/load-insns type Opcodes/ALOAD) index))

(defmethod emit-expr* :local [mv {:keys [type index context]}]
  (when-not (= context :statement)
    (emit-load mv type index)))

(defn emit-arithmetic [^MethodVisitor mv {:keys [type lhs rhs context]} op]
  (let [opcode (get-in insns/arithmetic-insns [type op])]
    (emit-expr mv lhs)
    (emit-expr mv rhs)
    (.visitInsn mv opcode)
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

(defmethod emit-expr* :neg [^MethodVisitor mv {:keys [type operand context]}]
  (emit-expr mv operand)
  (.visitInsn mv (get insns/negation-insns type))
  (drop-if-statement mv context))

(defmethod emit-expr* :bitwise-and [mv expr]
  (emit-arithmetic mv expr :bitwise-and))

(defmethod emit-expr* :bitwise-or [mv expr]
  (emit-arithmetic mv expr :bitwise-or))

(defmethod emit-expr* :bitwise-xor [mv expr]
  (emit-arithmetic mv expr :bitwise-xor))

(defmethod emit-expr* :shift-left [mv expr]
  (emit-arithmetic mv expr :shift-left))

(defmethod emit-expr* :shift-right [mv expr]
  (emit-arithmetic mv expr :shift-right))

(defmethod emit-expr* :logical-shift-right [mv expr]
  (emit-arithmetic mv expr :logical-shift-right))

(defmethod emit-expr* :widening-primitive [^MethodVisitor mv {:keys [type src context]}]
  (if (and (= (:op src) :literal) (= type t/LONG))
    (emit-expr mv (assoc src :context context :type type))
    (do (emit-expr mv src)
        (when-let [opcode (get-in insns/widening-insns [(:type src) type])]
          (.visitInsn mv opcode))
        (drop-if-statement mv context))))

(defmethod emit-expr* :narrowing-primitive [^MethodVisitor mv {:keys [type src context]}]
  (if (and (= (:op src) :literal) (#{t/BYTE t/SHORT t/CHAR} type))
    (emit-expr mv (assoc src :context context :type type))
    (do (emit-expr mv src)
        (case type
          (byte char short)
          (do (when-let [opcode (get-in insns/narrowing-insns [(:type src) t/INT])]
                (.visitInsn mv opcode))
              (.visitInsn mv (get-in insns/narrowing-insns [t/INT type])))
          (.visitInsn mv (get-in insns/narrowing-insns [(:type src) type])))
        (drop-if-statement mv context))))

(defmethod emit-expr* :boxing [mv {:keys [type src context]}]
  (emit-expr mv {:op :method-invocation
                 :context context
                 :class type
                 :interface? false
                 :type type
                 :access #{:public :static}
                 :arg-types [(:type src)]
                 :name "valueOf"
                 :args [src]}))

(def unboxing-method-names
  {t/BOOLEAN "booleanValue"
   t/BYTE "byteValue"
   t/CHAR "charValue"
   t/SHORT "shortValue"
   t/INT "intValue"
   t/LONG "longValue"
   t/FLOAT "floatValue"
   t/DOUBLE "doubleValue"})

(defmethod emit-expr* :unboxing [mv {:keys [type src context]}]
  (emit-expr mv {:op :method-invocation
                 :context context
                 :class (:type src)
                 :interface? false
                 :type type
                 :access #{:public}
                 :arg-types []
                 :name (unboxing-method-names type)
                 :target src
                 :args []}))

(defmethod emit-expr* :widening-reference [mv {:keys [src]}]
  (emit-expr mv src))

(defmethod emit-expr* :narrowing-reference [^MethodVisitor mv {:keys [type src context]}]
  (emit-expr mv src)
  (.visitTypeInsn mv Opcodes/CHECKCAST (.getInternalName ^Type type)))

(defmethod emit-expr* :instance? [^MethodVisitor mv {:keys [class operand context]}]
  (emit-expr mv operand)
  (.visitTypeInsn mv Opcodes/INSTANCEOF (.getInternalName ^Type class))
  (drop-if-statement mv context))

(defn emit-store [^MethodVisitor mv {:keys [type index]}]
  (.visitVarInsn mv (get insns/store-insns type Opcodes/ASTORE) index))

(defmethod emit-expr* :let [mv {:keys [bindings body]}]
  (doseq [{:keys [init] :as b} bindings]
    (emit-expr mv init)
    (emit-store mv b))
  (emit-expr mv body))

(defn emit-dup [^MethodVisitor mv type]
  (let [opcode (case type
               (long double) Opcodes/DUP2
               Opcodes/DUP)]
    (.visitInsn mv opcode)))

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
        (if-let [[opcode1 opcode2] (get-in insns/comparison-insns [t op])]
          (if opcode2
            (do (.visitInsn mv opcode1)
                (.visitJumpInsn mv opcode2 label))
            (.visitJumpInsn mv opcode1 label))
          (let [opcode (case op
                       :eq Opcodes/IF_ACMPNE
                       :ne Opcodes/IF_ACMPEQ)]
            (.visitJumpInsn mv opcode label))))
      (:method-invocation :instance?)
      (do (emit-expr mv cond)
          (.visitJumpInsn mv Opcodes/IFEQ label))
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
  (let [opcode (if target Opcodes/GETFIELD Opcodes/GETSTATIC)
        owner (.getInternalName ^Type class)
        desc (.getDescriptor ^Type type)]
    (.visitFieldInsn mv opcode owner name desc)
    (drop-if-statement mv context)))

(defmethod emit-expr* :field-update
  [^MethodVisitor mv {:keys [type name class target rhs context]}]
  (when target
    (emit-expr mv target))
  (emit-expr mv rhs)
  (when-not (= context :statement)
    (let [t (:type rhs)]
      (if target
        (let [opcode (if (= (t/type-category t) 2) Opcodes/DUP_X2 Opcodes/DUP_X1)]
          (.visitInsn mv opcode))
        (dup-unless-statement mv context t))))
  (let [opcode (if target Opcodes/PUTFIELD Opcodes/PUTSTATIC)
        owner (.getInternalName ^Type class)
        desc (.getDescriptor ^Type type)]
    (.visitFieldInsn mv opcode owner name desc)))

(defmethod emit-expr* :method-invocation
  [^MethodVisitor mv {:keys [interface? type name access class arg-types target args context]}]
  (when target
    (emit-expr mv target))
  (doseq [arg args]
    (emit-expr mv arg))
  (let [method-type (Type/getMethodType ^Type type (into-array Type arg-types))
        opcode (cond (:private access) Opcodes/INVOKESPECIAL
                   interface? Opcodes/INVOKEINTERFACE
                   target Opcodes/INVOKEVIRTUAL
                   :else Opcodes/INVOKESTATIC)
        iname (.getInternalName ^Type class)
        desc (.getDescriptor method-type)]
    (.visitMethodInsn mv opcode iname name desc interface?))
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

(defmethod emit-expr* :array-length [^MethodVisitor mv {:keys [array context]}]
  (emit-expr mv array)
  (.visitInsn mv Opcodes/ARRAYLENGTH)
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
