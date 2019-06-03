(ns jise.insns
  (:require [jise.type :as t])
  (:import [clojure.asm Opcodes]))

(def const-insns
  {t/BOOLEAN {true Opcodes/ICONST_1, false Opcodes/ICONST_0}
   t/INT {-1 Opcodes/ICONST_M1, 0 Opcodes/ICONST_0
          1 Opcodes/ICONST_1, 2 Opcodes/ICONST_2
          3 Opcodes/ICONST_3,4 Opcodes/ICONST_4
          5 Opcodes/ICONST_5}
   t/LONG {0 Opcodes/LCONST_0, 1 Opcodes/LCONST_1}
   t/FLOAT {0.0 Opcodes/FCONST_0, 1.0 Opcodes/FCONST_1
            2.0 Opcodes/FCONST_2}
   t/DOUBLE {0.0 Opcodes/DCONST_0, 1.0 Opcodes/DCONST_1}})

(def arithmetic-insns
  {:add Opcodes/IADD
   :sub Opcodes/ISUB
   :mul Opcodes/IMUL
   :div Opcodes/IDIV
   :rem Opcodes/IREM
   :bitwise-and Opcodes/IAND
   :bitwise-or Opcodes/IOR
   :bitwise-xor Opcodes/IXOR
   :shift-left Opcodes/ISHL
   :shift-right Opcodes/ISHR
   :logical-shift-right Opcodes/IUSHR})

(def comparison-insns
  {t/BOOLEAN {:eq [Opcodes/IF_ICMPNE], :ne [Opcodes/IF_ICMPEQ]}
   t/INT {:eq [Opcodes/IF_ICMPNE], :ne [Opcodes/IF_ICMPEQ]
          :lt [Opcodes/IF_ICMPGE], :gt [Opcodes/IF_ICMPLE]
          :le [Opcodes/IF_ICMPGT], :ge [Opcodes/IF_ICMPLT]}
   t/LONG {:eq [Opcodes/LCMP Opcodes/IFNE], :ne [Opcodes/LCMP Opcodes/IFEQ]
           :lt [Opcodes/LCMP Opcodes/IFGE], :gt [Opcodes/LCMP Opcodes/IFLE]
           :le [Opcodes/LCMP Opcodes/IFGT], :ge [Opcodes/LCMP Opcodes/IFLT]}
   t/FLOAT {:eq [Opcodes/FCMPL Opcodes/IFNE], :ne [Opcodes/FCMPL Opcodes/IFEQ]
            :lt [Opcodes/FCMPG Opcodes/IFGE], :gt [Opcodes/FCMPL Opcodes/IFLE]
            :le [Opcodes/FCMPG Opcodes/IFGT], :ge [Opcodes/FCMPL Opcodes/IFLT]}
   t/DOUBLE {:eq [Opcodes/DCMPL Opcodes/IFNE], :ne [Opcodes/DCMPL Opcodes/IFEQ]
             :lt [Opcodes/DCMPG Opcodes/IFGE], :gt [Opcodes/DCMPL Opcodes/IFLE]
             :le [Opcodes/DCMPG Opcodes/IFGT], :ge [Opcodes/DCMPL Opcodes/IFLT]}})

(def constant-comparison-insns
  {:eq-null Opcodes/IFNONNULL :ne-null Opcodes/IFNULL
   :eq-0 Opcodes/IFNE :ne-0 Opcodes/IFEQ
   :lt-0 Opcodes/IFGE :gt-0 Opcodes/IFLE
   :le-0 Opcodes/IFGT :ge-0 Opcodes/IFLT})

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
