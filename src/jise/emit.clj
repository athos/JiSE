(ns jise.emit
  (:require [jise.insns :as insns]
            [jise.type :as t])
  (:import [clojure.asm ClassVisitor ClassWriter Label MethodVisitor Opcodes Type]
           [clojure.lang Compiler DynamicClassLoader]))

(set! *warn-on-reflection* true)

(defn make-emitter [mv]
  {:mv mv
   :continue-label nil
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

(defmulti emit-expr* (fn [emitter expr] (:op expr)))
(defmethod emit-expr* :default [_ expr]
  (throw (ex-info (str "unknown expr found: " expr) {:expr expr})))

(defn emit-line [{:keys [^MethodVisitor mv]} line]
  (when line
    (let [here (Label.)]
      (.visitLabel mv here)
      (.visitLineNumber mv line here))))

(defn emit-return [{:keys [^MethodVisitor mv]} type]
  (.visitInsn mv (get insns/return-insns type Opcodes/ARETURN)))

(defn emit-expr [{:keys [^MethodVisitor mv] :as emitter} expr]
  (emit-expr* emitter expr)
  (when (= (:context expr) :return)
    (emit-return emitter (:type expr))))

(defn emit-ctor-invocation [{:keys [^MethodVisitor mv] :as emitter} {:keys [class arg-types args line]}]
  (let [method-type (Type/getMethodType t/VOID (into-array Type arg-types))
        iname (.getInternalName ^Type class)
        desc (.getDescriptor ^Type method-type)]
    (doseq [arg args]
      (emit-expr emitter arg))
    (emit-line emitter line)
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL iname "<init>" desc false)))

(defn emit-method [^ClassWriter cw parent ctor? {:keys [name access return-type args body]}]
  (let [desc (->> (map :type args)
                  (into-array Type)
                  (Type/getMethodDescriptor return-type))
        mname (if ctor? "<init>" (munge name))
        mv (.visitMethod cw (access-value access) mname desc nil nil)
        emitter (make-emitter mv)]
    (doseq [arg args]
      (.visitParameter mv (:name arg) (access-value (:access arg))))
    (.visitCode mv)
    ;; FIXME: it might be better to inject implicit ctor invocation in parsing phase
    (when (and ctor? (not= (get-in body [:exprs 0 :op]) :ctor-invocation))
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (emit-ctor-invocation emitter {:class parent :arg-types [] :args []}))
    (emit-expr emitter body)
    (when (= return-type t/VOID)
      (emit-return emitter t/VOID))
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
      (emit-method cw parent true ctor))
    (doseq [method methods]
      (emit-method cw parent false method))
    (.visitEnd cw)
    (.toByteArray cw)))

(defmethod emit-expr* :do [emitter {:keys [exprs]}]
  (doseq [expr exprs]
    (emit-expr emitter expr)))

(defn drop-if-statement [{:keys [^MethodVisitor mv]} context]
  (when (= context :statement)
    (let [opcode (if (= (t/type-category type) 2)
                 Opcodes/POP2
                 Opcodes/POP)]
      (.visitInsn mv opcode))))

(defmethod emit-expr* :null [{:keys [^MethodVisitor mv]} {:keys [context]}]
  (when-not (= context :statement)
    (.visitInsn mv Opcodes/ACONST_NULL)))

(defn primitive-type [type]
  (if (#{t/BYTE t/CHAR t/SHORT} type) t/INT type))

(defmethod emit-expr* :literal [{:keys [^MethodVisitor mv]} {:keys [type value context]}]
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

(defn emit-load [{:keys [^MethodVisitor mv]} type index]
  (.visitVarInsn mv (get insns/load-insns type Opcodes/ALOAD) index))

(defmethod emit-expr* :local [emitter {:keys [type index context]}]
  (when-not (= context :statement)
    (emit-load emitter type index)))

(defn emit-arithmetic [{:keys [^MethodVisitor mv] :as emitter} {:keys [type lhs rhs context line]} op]
  (let [opcode (get-in insns/arithmetic-insns [type op])]
    (emit-expr emitter lhs)
    (emit-expr emitter rhs)
    (emit-line emitter line)
    (.visitInsn mv opcode)
    (drop-if-statement emitter context)))

(defmethod emit-expr* :add [emitter expr]
  (emit-arithmetic emitter expr :add))

(defmethod emit-expr* :sub [emitter expr]
  (emit-arithmetic emitter expr :sub))

(defmethod emit-expr* :mul [emitter expr]
  (emit-arithmetic emitter expr :mul))

(defmethod emit-expr* :div [emitter expr]
  (emit-arithmetic emitter expr :div))

(defmethod emit-expr* :rem [emitter expr]
  (emit-arithmetic emitter expr :rem))

(defmethod emit-expr* :neg [{:keys [^MethodVisitor mv] :as emitter} {:keys [type operand context line]}]
  (emit-expr emitter operand)
  (emit-line emitter line)
  (.visitInsn mv (get insns/negation-insns type))
  (drop-if-statement emitter context))

(defmethod emit-expr* :bitwise-and [emitter expr]
  (emit-arithmetic emitter expr :bitwise-and))

(defmethod emit-expr* :bitwise-or [emitter expr]
  (emit-arithmetic emitter expr :bitwise-or))

(defmethod emit-expr* :bitwise-xor [emitter expr]
  (emit-arithmetic emitter expr :bitwise-xor))

(defmethod emit-expr* :shift-left [emitter expr]
  (emit-arithmetic emitter expr :shift-left))

(defmethod emit-expr* :shift-right [emitter expr]
  (emit-arithmetic emitter expr :shift-right))

(defmethod emit-expr* :logical-shift-right [emitter expr]
  (emit-arithmetic emitter expr :logical-shift-right))

(defmethod emit-expr* :widening-primitive [{:keys [^MethodVisitor mv] :as emitter} {:keys [type src context]}]
  (if (and (= (:op src) :literal) (= type t/LONG))
    (emit-expr emitter (assoc src :context context :type type))
    (do (emit-expr emitter src)
        (when-let [opcode (get-in insns/widening-insns [(:type src) type])]
          (.visitInsn mv opcode))
        (drop-if-statement emitter context))))

(defmethod emit-expr* :narrowing-primitive [{:keys [^MethodVisitor mv] :as emitter} {:keys [type src context]}]
  (if (and (= (:op src) :literal) (#{t/BYTE t/SHORT t/CHAR} type))
    (emit-expr emitter (assoc src :context context :type type))
    (do (emit-expr emitter src)
        (case type
          (byte char short)
          (do (when-let [opcode (get-in insns/narrowing-insns [(:type src) t/INT])]
                (.visitInsn mv opcode))
              (.visitInsn mv (get-in insns/narrowing-insns [t/INT type])))
          (.visitInsn mv (get-in insns/narrowing-insns [(:type src) type])))
        (drop-if-statement emitter context))))

(defmethod emit-expr* :boxing [emitter {:keys [type src context]}]
  (emit-expr emitter {:op :method-invocation
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

(defmethod emit-expr* :unboxing [emitter {:keys [type src context]}]
  (emit-expr emitter {:op :method-invocation
                      :context context
                      :class (:type src)
                      :interface? false
                      :type type
                      :access #{:public}
                      :arg-types []
                      :name (unboxing-method-names type)
                      :target src
                      :args []}))

(defmethod emit-expr* :widening-reference [emitter {:keys [src]}]
  (emit-expr emitter src))

(defmethod emit-expr* :narrowing-reference [{:keys [^MethodVisitor mv] :as emitter} {:keys [type src context]}]
  (emit-expr emitter src)
  (.visitTypeInsn mv Opcodes/CHECKCAST (.getInternalName ^Type type))
  (drop-if-statement emitter context))

(defmethod emit-expr* :instance? [{:keys [^MethodVisitor mv] :as emitter} {:keys [class operand context line]}]
  (emit-expr emitter operand)
  (emit-line emitter line)
  (.visitTypeInsn mv Opcodes/INSTANCEOF (.getInternalName ^Type class))
  (drop-if-statement emitter context))

(defn emit-store [{:keys [^MethodVisitor mv]} {:keys [type index]}]
  (.visitVarInsn mv (get insns/store-insns type Opcodes/ASTORE) index))

(defmethod emit-expr* :let [emitter {:keys [bindings body line]}]
  (emit-line emitter line)
  (doseq [{:keys [init] :as b} bindings]
    (emit-expr emitter init)
    (emit-store emitter b))
  (emit-expr emitter body))

(defn emit-dup [{:keys [^MethodVisitor mv]} type]
  (let [opcode (case type
               (long double) Opcodes/DUP2
               Opcodes/DUP)]
    (.visitInsn mv opcode)))

(defn dup-unless-statement [emitter context type]
  (when-not (= context :statement)
    (emit-dup emitter type)))

(defmethod emit-expr* :assignment [emitter {:keys [lhs rhs context line]}]
  (emit-expr emitter rhs)
  (dup-unless-statement emitter context (:type rhs))
  (emit-line emitter line)
  (emit-store emitter lhs))

(defmethod emit-expr* :increment [{:keys [^MethodVisitor mv] :as emitter} {:keys [target by context line]}]
  (let [{:keys [type index]} target]
    (emit-line emitter line)
    (.visitIincInsn mv index by)
    (when-not (= context :statement)
      (emit-load emitter type index))))

(defmethod emit-expr* :labeled [{:keys [^MethodVisitor mv] :as emitter} {:keys [label target kind]}]
  (let [break-label (Label.)
        emitter' (assoc-in emitter [:labels label] {:break-label break-label})]
    (emit-expr emitter' target)
    (.visitLabel mv break-label)))

(defn emit-comparison [{:keys [^MethodVisitor mv] :as emitter} op {:keys [lhs rhs]} label]
  (let [t (:type lhs)]
    (emit-expr emitter lhs)
    (emit-expr emitter rhs)
    (if-let [[opcode1 opcode2] (get-in insns/comparison-insns [t op])]
      (if opcode2
        (do (.visitInsn mv opcode1)
            (.visitJumpInsn mv opcode2 label))
        (.visitJumpInsn mv opcode1 label))
      (let [opcode (case op
                     :eq Opcodes/IF_ACMPNE
                     :ne Opcodes/IF_ACMPEQ)]
        (.visitJumpInsn mv opcode label)))))

(declare emit-conditional)

(defn emit-and [emitter {:keys [exprs]} label]
  (run! #(emit-conditional emitter % label) exprs))

(defn emit-or [{:keys [^MethodVisitor mv] :as emitter} {:keys [exprs expr]} else-label]
  (let [then-label (Label.)]
    (run! #(emit-conditional emitter % then-label) exprs)
    (emit-conditional emitter expr else-label)
    (.visitLabel mv then-label)))

(def negated-comparison-ops
  {:eq :ne, :ne :eq, :lt :ge, :gt :le, :le :gt, :ge :lt})

(defn emit-not [{:keys [^MethodVisitor mv] :as emitter} {:keys [expr]} label]
  (if-let [negated (negated-comparison-ops (:op expr))]
    (emit-comparison emitter negated expr label)
    (do (emit-expr emitter expr)
        (.visitJumpInsn mv Opcodes/IFNE label))))

(defn emit-conditional [{:keys [^MethodVisitor mv] :as emitter} cond label]
  (let [op (:op cond)]
    (case op
      (:eq :ne :lt :gt :le :ge)
      (emit-comparison emitter op cond label)
      :and
      (emit-and emitter cond label)
      :or
      (emit-or emitter cond label)
      :not
      (emit-not emitter cond label)
      (:method-invocation :instance?)
      (do (emit-expr emitter cond)
          (.visitJumpInsn mv Opcodes/IFEQ label))
      (let [msg (str "not supported conditional: " op)]
        (throw (ex-info msg {:op op}))))))

(defmethod emit-expr* :if [{:keys [^MethodVisitor mv] :as emitter} {:keys [test then else line]}]
  (let [end-label (Label.)
        else-label (if else (Label.) end-label)]
    (emit-line emitter line)
    (emit-conditional emitter test else-label)
    (emit-expr emitter then)
    (when else
      (.visitJumpInsn mv Opcodes/GOTO end-label)
      (.visitLabel mv else-label)
      (emit-expr emitter else))
    (.visitLabel mv end-label)))

(defmethod emit-expr* :switch [{:keys [^MethodVisitor mv] :as emitter} {:keys [test clauses default]}]
  (let [end-label (Label.)
        default-label (if default (Label.) end-label)
        clauses' (map #(assoc % :label (Label.)) clauses)
        key->label (->> (for [{:keys [keys label]} clauses'
                              key keys]
                          [key label])
                        (sort-by first))
        keys (int-array (map first key->label))
        labels (into-array Label (map second key->label))]
    (emit-expr emitter test)
    (.visitLookupSwitchInsn mv default-label keys labels)
    (doseq [{:keys [label guard body]} clauses']
      (.visitLabel mv label)
      (when guard
        (emit-conditional emitter guard default-label))
      (emit-expr emitter body)
      (.visitJumpInsn mv Opcodes/GOTO end-label))
    (when default
      (.visitLabel mv default-label)
      (emit-expr emitter default))
    (.visitLabel mv end-label)))

(defn with-labels [emitter label-name continue-label break-label f]
  (let [emitter' (-> emitter
                     (assoc :continue-label continue-label :break-label break-label)
                     (cond-> label-name (assoc-in [:labels label-name :continue-label] continue-label)))]
    (f emitter')))

(defmethod emit-expr* :while [{:keys [^MethodVisitor mv] :as emitter} {:keys [cond body label context line]}]
  (let [start-label (Label.)
        end-label (Label.)]
    (with-labels emitter label start-label end-label
      (fn [emitter']
        (.visitLabel mv start-label)
        (emit-line emitter' line)
        (emit-conditional emitter' cond end-label)
        (emit-expr emitter' body)
        (.visitJumpInsn mv Opcodes/GOTO start-label)
        (.visitLabel mv end-label)))
    (when-not (= context :statement)
      (.visitInsn mv Opcodes/ACONST_NULL))))

(defmethod emit-expr* :for [{:keys [^MethodVisitor mv] :as emitter} {:keys [cond step body label context]}]
  (let [start-label (Label.)
        continue-label (Label.)
        end-label (Label.)]
    (with-labels emitter label continue-label end-label
      (fn [emitter']
        (.visitLabel mv start-label)
        (emit-conditional emitter' cond end-label)
        (emit-expr emitter' body)
        (.visitLabel mv continue-label)
        (emit-expr emitter' step)
        (.visitJumpInsn mv Opcodes/GOTO start-label)
        (.visitLabel mv end-label)))
    (when-not (= context :statement)
      (.visitInsn mv Opcodes/ACONST_NULL))))

(defmethod emit-expr* :continue [{:keys [^MethodVisitor mv] :as emitter} {:keys [label]}]
  (let [^Label label (if label
                       (get-in emitter [:labels label :continue-label])
                       (:continue-label emitter))]
    (.visitJumpInsn mv Opcodes/GOTO label)))

(defmethod emit-expr* :break [{:keys [^MethodVisitor mv] :as emitter} {:keys [label]}]
  (let [^Label label (if label
                       (get-in emitter [:labels label :break-label])
                       (:break-label emitter))]
    (.visitJumpInsn mv Opcodes/GOTO label)))

(defmethod emit-expr* :throw [{:keys [^MethodVisitor mv] :as emitter} {:keys [exception]}]
  (emit-expr emitter exception)
  (.visitInsn mv Opcodes/ATHROW))

(defmethod emit-expr* :new [{:keys [^MethodVisitor mv] :as emitter} {:keys [type context] :as expr}]
  (.visitTypeInsn mv Opcodes/NEW (.getInternalName ^Type type))
  (dup-unless-statement emitter context type)
  (emit-ctor-invocation emitter (assoc expr :class type)))

(defmethod emit-expr* :field-access
  [{:keys [^MethodVisitor mv] :as emitter} {:keys [type name class target context line]}]
  (when target
    (emit-expr emitter target))
  (let [opcode (if target Opcodes/GETFIELD Opcodes/GETSTATIC)
        owner (.getInternalName ^Type class)
        desc (.getDescriptor ^Type type)]
    (emit-line emitter line)
    (.visitFieldInsn mv opcode owner name desc)
    (drop-if-statement emitter context)))

(defmethod emit-expr* :field-update
  [{:keys [^MethodVisitor mv] :as emitter} {:keys [type name class target rhs context line]}]
  (when target
    (emit-expr emitter target))
  (emit-expr emitter rhs)
  (when-not (= context :statement)
    (let [t (:type rhs)]
      (if target
        (let [opcode (if (= (t/type-category t) 2) Opcodes/DUP_X2 Opcodes/DUP_X1)]
          (.visitInsn mv opcode))
        (dup-unless-statement emitter context t))))
  (let [opcode (if target Opcodes/PUTFIELD Opcodes/PUTSTATIC)
        owner (.getInternalName ^Type class)
        desc (.getDescriptor ^Type type)]
    (emit-line emitter line)
    (.visitFieldInsn mv opcode owner name desc)))

(defmethod emit-expr* :ctor-invocation [emitter {:keys [class] :as expr}]
  (emit-load emitter class 0)
  (emit-ctor-invocation emitter expr))

(defmethod emit-expr* :method-invocation
  [{:keys [^MethodVisitor mv] :as emitter}
   {:keys [interface? type name access class arg-types target args context line]}]
  (when target
    (emit-expr emitter target))
  (doseq [arg args]
    (emit-expr emitter arg))
  (let [method-type (Type/getMethodType ^Type type (into-array Type arg-types))
        opcode (cond (:private access) Opcodes/INVOKESPECIAL
                   interface? Opcodes/INVOKEINTERFACE
                   target Opcodes/INVOKEVIRTUAL
                   :else Opcodes/INVOKESTATIC)
        iname (.getInternalName ^Type class)
        desc (.getDescriptor method-type)]
    (emit-line emitter line)
    (.visitMethodInsn mv opcode iname name desc interface?))
  (when-not (= type t/VOID)
    (drop-if-statement emitter context)))

(def primitive-types
  {t/BOOLEAN Opcodes/T_BOOLEAN
   t/BYTE Opcodes/T_BYTE
   t/CHAR Opcodes/T_CHAR
   t/SHORT Opcodes/T_SHORT
   t/INT Opcodes/T_INT
   t/LONG Opcodes/T_LONG
   t/FLOAT Opcodes/T_FLOAT
   t/DOUBLE Opcodes/T_DOUBLE})

(defmethod emit-expr* :new-array [{:keys [^MethodVisitor mv] :as emitter} {:keys [type length context line]}]
  (emit-expr emitter length)
  (emit-line emitter line)
  (let [elem-type (t/element-type type)]
    (if (t/primitive-type? elem-type)
      (let [t (primitive-types elem-type)]
        (.visitIntInsn mv Opcodes/NEWARRAY t))
      (.visitTypeInsn mv Opcodes/ANEWARRAY (.getInternalName elem-type))))
  (drop-if-statement emitter context))

(defmethod emit-expr* :array-length [{:keys [^MethodVisitor mv] :as emitter} {:keys [array context line]}]
  (emit-expr emitter array)
  (emit-line emitter line)
  (.visitInsn mv Opcodes/ARRAYLENGTH)
  (drop-if-statement emitter context))

(defmethod emit-expr* :array-access [{:keys [^MethodVisitor mv] :as emitter} {:keys [array index context line]}]
  (emit-expr emitter array)
  (emit-expr emitter index)
  (emit-line emitter line)
  (let [elem-type (t/element-type (:type array))]
    (.visitInsn mv (get insns/aload-insns elem-type Opcodes/AALOAD))
    (drop-if-statement emitter context)))

(defmethod emit-expr* :array-update
  [{:keys [^MethodVisitor mv] :as emitter} {:keys [array index expr context line]}]
  (emit-expr emitter array)
  (emit-expr emitter index)
  (emit-expr emitter expr)
  (dup-unless-statement emitter context (:type expr))
  (emit-line emitter line)
  (let [elem-type (t/element-type (:type array))]
    (.visitInsn mv (get insns/astore-insns elem-type Opcodes/AASTORE))))
