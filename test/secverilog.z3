; this part encodes a partial order on labels
(declare-sort Label)
(declare-fun leq (Label Label) Bool)
(declare-fun join (Label Label) Label)
(declare-fun meet (Label Label) Label)
(assert (forall ((x Label)) (leq x x)))
(assert (forall ((x Label) (y Label) (z Label)) (implies (and (leq x y) (leq y z)) (leq x z))))
(assert (forall ((x Label) (y Label)) (implies (and (leq x y) (leq y x)) (= x y))))

; axioms for join
(assert (forall ((x Label) (y Label) (z Label)) (implies (leq (join x y) z) (and (leq x z) (leq y z)))))
(assert (forall ((x Label) (y Label) (z Label)) (implies (and (leq x z) (leq y z)) (leq (join x y) z))))
(assert (forall ((x Label) (y Label)) (and (leq x (join x y)) (leq y (join x y)))))
(assert (forall ((x Label) (y Label)) (= (join x y) (join y x))))

; axioms for meet
(assert (forall ((x Label) (y Label) (z Label)) (implies (leq x (meet y z)) (and (leq x y) (leq x z)))))
(assert (forall ((x Label) (y Label) (z Label)) (implies (and (leq x y) (leq x z)) (leq x (meet y z)))))
(assert (forall ((x Label) (y Label)) (and (leq (meet x y) x) (leq (meet x y) y))))
(assert (forall ((x Label) (y Label)) (= (meet x y) (meet y x))))

; lattice elements
(declare-fun LOW () Label)
(declare-fun HIGH () Label)
(declare-fun D1 () Label)
(declare-fun D2 () Label)

; lattice structure
(assert (forall ((x Label)) (leq LOW x)))
(assert (forall ((x Label)) (leq x HIGH)))
(assert (not (= HIGH LOW))) ; the lattice cannot clapse

; variables to be solved
(declare-fun state () Int)
(assert (<= 0  state))
(assert (<= state 3))

; function that maps 0 to LOW; 1 to HIGH
(declare-fun LH (Int) Label)
(assert (= (LH 0) LOW))
(assert (= (LH 1) HIGH))
; ZL(0)=L, ZL(1)=H, ZL(2)=H, ZL(3)=H
(declare-fun ZL (Int) Label)
(assert (= (ZL 0) LOW))
(assert (= (ZL 1) HIGH))
(assert (= (ZL 2) HIGH))
(assert (= (ZL 3) HIGH))

; LH(0)=L, LH(1)=H
(declare-fun LH (Int) Label)
(assert (= (LH 0) LOW))
(assert (= (LH 1) HIGH))

; assertions to be verified

(push)
(assert  (not(leq LOW  LOW)))    ; wallclock <= (wallclock)+(1) @secverilog.v:18
(check-sat)
(pop)


(push)
(assert (and (= state 0)  (not(leq (ZL state)  HIGH))))    ; inH <= in @secverilog.v:22
(check-sat)
(pop)


(push)
(assert (and (= state 0)  (not(leq (ZL state)  LOW))))    ; out <= 0 @secverilog.v:24
(check-sat)
(pop)


(push)
(declare-fun state25 () Int)
(assert (<= 0  state25))
(assert (<= state25 3))
(assert (and (= state 0) (= state25 1)  (not(leq (ZL state)  (ZL state25)))))    ; state <= 1 @secverilog.v:25
(check-sat)
(pop)


(push)
(assert (and (= state 0)  (not(leq (ZL state)  LOW))))    ; out <= wallclock @secverilog.v:27
(check-sat)
(pop)


(push)
(declare-fun state28 () Int)
(assert (<= 0  state28))
(assert (<= state28 3))
(assert (and (= state 0) (= state28 0)  (not(leq (ZL state)  (ZL state28)))))    ; state <= 0 @secverilog.v:28
(check-sat)
(pop)


(push)
(declare-fun state33 () Int)
(assert (<= 0  state33))
(assert (<= state33 3))
(assert (and (= state 0) (= state 1) (= state33 0)  (not(leq HIGH  (ZL state33)))))    ; state <= 0 @secverilog.v:33
(check-sat)
(pop)


(push)
(assert (and (= state 0) (= state 1)  (not(leq HIGH  HIGH))))    ; secret <= 0 @secverilog.v:34
(check-sat)
(pop)


(push)
(declare-fun state36 () Int)
(assert (<= 0  state36))
(assert (<= state36 3))
(assert (and (= state 0) (= state 1) (= state36 2)  (not(leq HIGH  (ZL state36)))))    ; state <= 2 @secverilog.v:36
(check-sat)
(pop)


(push)
(declare-fun state40 () Int)
(assert (<= 0  state40))
(assert (<= state40 3))
(assert (and (= state 0) (= state 1) (= state 2) (= state40 0)  (not(leq (ZL state)  (ZL state40)))))    ; state <= 0 @secverilog.v:40
(check-sat)
(pop)


(push)
(assert (and (= state 0) (= state 1) (= state 2)  (not(leq HIGH  HIGH))))    ; secret <= (secret)*(inH) @secverilog.v:41
(check-sat)
(pop)

