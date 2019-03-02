(ns jise.insns
  (:import [clojure.asm Opcodes Type]))

(def primitive-types
  {'boolean Type/BOOLEAN_TYPE
   'byte Type/BYTE_TYPE
   'char Type/CHAR_TYPE
   'short Type/SHORT_TYPE
   'int Type/INT_TYPE
   'long Type/LONG_TYPE
   'float Type/FLOAT_TYPE
   'double Type/DOUBLE_TYPE
   'void Type/VOID_TYPE})

(def const-insns
  {'boolean {true Opcodes/ICONST_1, false Opcodes/ICONST_0}
   'int {-1 Opcodes/ICONST_M1, 0 Opcodes/ICONST_0
         1 Opcodes/ICONST_1, 2 Opcodes/ICONST_2
         3 Opcodes/ICONST_3,4 Opcodes/ICONST_4
         5 Opcodes/ICONST_5}
   'long {0 Opcodes/LCONST_0, 1 Opcodes/LCONST_1}
   'float {0 Opcodes/FCONST_0, 1 Opcodes/FCONST_1
           2 Opcodes/FCONST_2}
   'double {0 Opcodes/DCONST_0, 1 Opcodes/DCONST_1}})

(def return-insns
  {'void Opcodes/RETURN
   'boolean Opcodes/IRETURN
   'byte Opcodes/IRETURN
   'char Opcodes/IRETURN
   'short Opcodes/IRETURN
   'int Opcodes/IRETURN
   'long Opcodes/LRETURN
   'float Opcodes/FRETURN
   'double Opcodes/DRETURN})

(def load-insns
  {'boolean Opcodes/ILOAD
   'byte Opcodes/ILOAD
   'char Opcodes/ILOAD
   'short Opcodes/ILOAD
   'int Opcodes/ILOAD
   'long Opcodes/LLOAD
   'float Opcodes/FLOAD
   'double Opcodes/DLOAD})

(def store-insns
  {'int Opcodes/ISTORE
   'long Opcodes/LSTORE
   'float Opcodes/FSTORE
   'double Opcodes/DSTORE})

(def arithmetic-insns
  {:add {'int Opcodes/IADD
         'long Opcodes/LADD
         'float Opcodes/FADD
         'double Opcodes/DADD}
   :sub {'int Opcodes/ISUB
         'long Opcodes/LSUB
         'float Opcodes/FSUB
         'double Opcodes/DSUB}
   :mul {'int Opcodes/IMUL
         'long Opcodes/LMUL
         'float Opcodes/FMUL
         'double Opcodes/DMUL}
   :div {'int Opcodes/IDIV
         'long Opcodes/LDIV
         'float Opcodes/FDIV
         'double Opcodes/DDIV}
   :rem {'int Opcodes/IREM
         'long Opcodes/LREM
         'float Opcodes/FREM
         'double Opcodes/DREM}})

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

(def conversion-insns
  {'int {'byte Opcodes/I2B
         'char Opcodes/I2C
         'short Opcodes/I2S
         'long Opcodes/I2L
         'float Opcodes/I2F
         'double Opcodes/I2D}
   'long {'int Opcodes/L2I
          'float Opcodes/L2F
          'double Opcodes/L2D}
   'float {'int Opcodes/F2I
           'long Opcodes/F2L
           'double Opcodes/F2D}
   'double {'int Opcodes/D2I
            'long Opcodes/D2L
            'float Opcodes/D2F}})

(def aload-insns
  {'boolean Opcodes/BALOAD
   'byte Opcodes/BALOAD
   'char Opcodes/CALOAD
   'short Opcodes/SALOAD
   'int Opcodes/IALOAD
   'long Opcodes/LALOAD
   'float Opcodes/FALOAD
   'double Opcodes/DALOAD})

(def astore-insns
  {'boolean Opcodes/BASTORE
   'byte Opcodes/BASTORE
   'char Opcodes/CASTORE
   'short Opcodes/SASTORE
   'int Opcodes/IASTORE
   'long Opcodes/LASTORE
   'float Opcodes/FASTORE
   'double Opcodes/DASTORE})
