query_naming(inv(if_inst, ex_inst, reg, done, t)).
% assignment of Y to X. (X->Y)
assign_op(X_T, Y_T) := ite(X_T=1, Y_T=1, Y_T=0).

% Some operation transforming X1 and X2 int Y. (represented by X1,X2 -O-> Y).
% We only track taint bits. Can later be modeled as uninterpreted function.
circle_op(X1_T, X2_T, Y_T) := ite((X1_T=1; X2_T=1), Y_T=1, Y_T=0).


next(IF_Inst, Ex_Inst, REG, Done, T,
     IF_Inst1, Ex_Inst1, REG1, Done1, T1) := 
   (
     % reset timer and taint IF_INST.
     Done=0, T1=0, Done1=0, IF_Inst1=1, Ex_Inst1=0, REG1=0
     % take a step and increment timer.
   ; Done=0, T1=T+1,
     % IF Stage
     IF_Inst1=0,
     % EX Stage
     assign_op(IF_Inst, Ex_Inst1),
     % WB Stage
     circle_op(Ex_Inst, 0, REG1),
     ite(REG1=1, Done1=1, Done1=Done)
   ; % done: spin.
     Done=1, T1=T, Done1=Done
   ).

inv(IF_Inst, Ex_Inst, REG, Done, T) :-
	IF_Inst=0, Ex_Inst=0, REG=0, Done=0, T=0.

inv(IF_Inst1, Ex_Inst1, REG1, Done1, T1) :-
	next(IF_Inst, Ex_Inst, REG, Done, T, IF_Inst1, Ex_Inst1, REG1, Done1, T1),
	inv(IF_Inst, Ex_Inst, REG, Done, T).

TR=TL :-
	inv(IF_InstL, Ex_InstL, REGL, DoneL, TL),
	inv(IF_InstR, Ex_InstR, REGR, DoneR, TR),
	DoneL=1, DoneR=1.