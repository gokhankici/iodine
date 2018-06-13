; ZL(0)=L, ZL(n > 0)=H
(declare-fun ZL (Int) Label)
(assert (= (ZL 0) LOW))
(assert (= (ZL 1) HIGH))
(assert (= (ZL 2) HIGH))
(assert (= (ZL 3) HIGH))
(assert (= (ZL 4) HIGH))
(assert (= (ZL 5) HIGH))
(assert (= (ZL 6) HIGH))

; LH(0)=L, LH(1)=H
(declare-fun LH (Int) Label)
(assert (= (LH 0) LOW))
(assert (= (LH 1) HIGH))
