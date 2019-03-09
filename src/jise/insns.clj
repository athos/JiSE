(ns jise.insns
  (:require [jise.type :as t])
  (:import [clojure.asm Opcodes Type]))

(def primitive-types
  {t/BOOLEAN Type/BOOLEAN_TYPE
   t/BYTE Type/BYTE_TYPE
   t/CHAR Type/CHAR_TYPE
   t/SHORT Type/SHORT_TYPE
   t/INT Type/INT_TYPE
   t/LONG Type/LONG_TYPE
   t/FLOAT Type/FLOAT_TYPE
   t/DOUBLE Type/DOUBLE_TYPE
   t/VOID Type/VOID_TYPE})

(def const-insns
  {t/BOOLEAN {true Opcodes/ICONST_1, false Opcodes/ICONST_0}
   t/INT {-1 Opcodes/ICONST_M1, 0 Opcodes/ICONST_0
          1 Opcodes/ICONST_1, 2 Opcodes/ICONST_2
          3 Opcodes/ICONST_3,4 Opcodes/ICONST_4
          5 Opcodes/ICONST_5}
   t/LONG {0 Opcodes/LCONST_0, 1 Opcodes/LCONST_1}
   t/FLOAT {0 Opcodes/FCONST_0, 1 Opcodes/FCONST_1
            2 Opcodes/FCONST_2}
   t/DOUBLE {0 Opcodes/DCONST_0, 1 Opcodes/DCONST_1}})

(def return-insns
  {t/VOID Opcodes/RETURN
   t/BOOLEAN Opcodes/IRETURN
   t/BYTE Opcodes/IRETURN
   t/CHAR Opcodes/IRETURN
   t/SHORT Opcodes/IRETURN
   t/INT Opcodes/IRETURN
   t/LONG Opcodes/LRETURN
   t/FLOAT Opcodes/FRETURN
   t/DOUBLE Opcodes/DRETURN})

(def load-insns
  {t/BOOLEAN Opcodes/ILOAD
   t/BYTE Opcodes/ILOAD
   t/CHAR Opcodes/ILOAD
   t/SHORT Opcodes/ILOAD
   t/INT Opcodes/ILOAD
   t/LONG Opcodes/LLOAD
   t/FLOAT Opcodes/FLOAD
   t/DOUBLE Opcodes/DLOAD})

(def store-insns
  {t/INT Opcodes/ISTORE
   t/LONG Opcodes/LSTORE
   t/FLOAT Opcodes/FSTORE
   t/DOUBLE Opcodes/DSTORE})

(def arithmetic-insns
  {t/INT {:add Opcodes/IADD
          :sub Opcodes/ISUB
          :mul Opcodes/IMUL
          :div Opcodes/IDIV
          :rem Opcodes/IREM}
   t/LONG {:add Opcodes/LADD
           :sub Opcodes/LSUB
           :mul Opcodes/LMUL
           :div Opcodes/LDIV
           :rem Opcodes/LREM}
   t/FLOAT {:add Opcodes/FADD
            :sub Opcodes/FSUB
            :mul Opcodes/FMUL
            :div Opcodes/FDIV
            :rem Opcodes/FREM}
   t/DOUBLE {:add Opcodes/DADD
             :sub Opcodes/DSUB
             :mul Opcodes/DMUL
             :div Opcodes/DDIV
             :rem Opcodes/DREM}})

(def comparison-insns
  {t/INT {:eq [Opcodes/IF_ICMPNE], :ne [Opcodes/IF_ICMPEQ]
          :lt [Opcodes/IF_ICMPGE], :gt [Opcodes/IF_ICMPLE]
          :le [Opcodes/IF_ICMPGT], :ge [Opcodes/IF_ICMPLT]}
   t/LONG {:eq [Opcodes/LCMP Opcodes/IFNE], :ne [Opcodes/LCMP Opcodes/IFEQ]
           :lt [Opcodes/LCMP Opcodes/IFGE], :gt [Opcodes/LCMP Opcodes/IFLE]
           :le [Opcodes/LCMP Opcodes/IFGT], :ge [Opcodes/LCMP Opcodes/IFLT]}
   t/FLOAT {:eq [Opcodes/FCMPL Opcodes/IFNE], :ne [Opcodes/FCMPL Opcodes/IFEQ]
            :lt [Opcodes/FCMPL Opcodes/IFGE], :gt [Opcodes/FCMPL Opcodes/IFLE]
            :le [Opcodes/FCMPL Opcodes/IFGT], :ge [Opcodes/FCMPL Opcodes/IFLT]}
   t/DOUBLE {:eq [Opcodes/DCMPL Opcodes/IFNE], :ne [Opcodes/DCMPL Opcodes/IFEQ]
             :lt [Opcodes/DCMPL Opcodes/IFGE], :gt [Opcodes/DCMPL Opcodes/IFLE]
             :le [Opcodes/DCMPL Opcodes/IFGT], :ge [Opcodes/DCMPL Opcodes/IFLT]}})

(def widening-insns
  {t/INT {t/LONG Opcodes/I2L
          t/FLOAT Opcodes/I2F
          t/DOUBLE Opcodes/I2D}
   t/LONG {t/FLOAT Opcodes/L2F
           t/DOUBLE Opcodes/L2D}
   t/FLOAT {t/DOUBLE Opcodes/F2D}})

(def narrowing-insns
  {t/INT {t/BYTE Opcodes/I2B
          t/CHAR Opcodes/I2C
          t/SHORT Opcodes/I2S}
   t/LONG {t/INT Opcodes/L2I}
   t/FLOAT {t/INT Opcodes/F2I
            t/LONG Opcodes/F2L}
   t/DOUBLE {t/INT Opcodes/D2I
             t/LONG Opcodes/D2L
             t/FLOAT Opcodes/D2F}})

(def aload-insns
  {t/BOOLEAN Opcodes/BALOAD
   t/BYTE Opcodes/BALOAD
   t/CHAR Opcodes/CALOAD
   t/SHORT Opcodes/SALOAD
   t/INT Opcodes/IALOAD
   t/LONG Opcodes/LALOAD
   t/FLOAT Opcodes/FALOAD
   t/DOUBLE Opcodes/DALOAD})

(def astore-insns
  {t/BOOLEAN Opcodes/BASTORE
   t/BYTE Opcodes/BASTORE
   t/CHAR Opcodes/CASTORE
   t/SHORT Opcodes/SASTORE
   t/INT Opcodes/IASTORE
   t/LONG Opcodes/LASTORE
   t/FLOAT Opcodes/FASTORE
   t/DOUBLE Opcodes/DASTORE})
