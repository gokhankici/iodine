% vim: set ft=prolog

query_naming(
	     inv(
		 id_instrl, if_inst_tl, id_instr_tl, mem_aluout_tl, donel, tl,
		 id_instrr, if_inst_tr, id_instr_tr, mem_aluout_tr, doner, tr
		)
	    ).

% assignment of Y to X. (X->Y)
assign_op(X_T, Y_T) := X_T=Y_T. %ite(X_T>=1, Y_T>=1, Y_T=<0).

% Some operation transforming X into Y. (represented by f_-O-> Y).
% We only track taint bits. Can later be modeled as uninterpreted function.
%circle_op(X_T, Y_T) := ite(X_T=1, Y_T=1, Y_T=0).

next(
     Stall, Cond1, If_Instr, Id_Instr, If_inst_t, Id_instr_t, Ex_aluout_t, Done, T,
     Id_Instr1, Id_instr_t1, Mem_aluout_t1, Done1, T1
    ) := 
(
     % wiring
     assign_op(Id_instr_t, Ex_aluout_t),
     %-- Hazard detection
     ite(Cond1=1, Stall=1, Stall=0),
     Done=0,
     % DEC Stage
     (   Stall=1, assign_op(Id_instr_t, Id_instr_t1), Id_Instr1=Id_Instr
     ;   Stall=0, assign_op(If_inst_t, Id_instr_t1) , Id_Instr1=If_Instr
     ),
     % WB Stage
     assign_op(Ex_aluout_t, Mem_aluout_t1),
     ite(Mem_aluout_t1>=1, Done1=1, Done1=Done)
).

inv(
    Id_InstrL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, TL,
    Id_InstrR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR, TR
   ) :-
	If_inst_tL=1, Id_instr_tL=0, Mem_aluout_tL=0, DoneL=0, TL=0,
	If_inst_tR=1, Id_instr_tR=0, Mem_aluout_tR=0, DoneR=0, TR=0,
	%-- is this okay?
	Id_InstrL=Id_InstrR.	

inv(
    Id_InstrL1, If_inst_tL1, Id_instr_tL1, Mem_aluout_tL1, DoneL1, TL1,
    Id_InstrR1, If_inst_tR1, Id_instr_tR1, Mem_aluout_tR1, DoneR1, TR1
   ) :-
	(
	    % both executions have not finished yet: issue new a taint bit, register valuations.
	    DoneL=0, TL1=0, DoneL1=0, 
	    DoneR=0, TR1=0, DoneR1=0, 
	    % all variable valuations stay the same.
	  Id_InstrL1=Id_InstrL, If_inst_tL1=1, Id_instr_tL1=0,
	  Id_InstrR1=Id_InstrR, If_inst_tR1=1, Id_instr_tR1=0
	;   % both not done: both executions take a step.
	  next(
	       StallL, Cond1L, If_InstrL, Id_InstrL, If_inst_tL, Id_instr_tL, Ex_aluout_tL, DoneL, TL,
	       Id_InstrL1, Id_instr_tL1, Mem_aluout_tL1, DoneL1, TL1
	      ),
	  next(
	       StallR, Cond1R, If_InstrR, Id_InstrR, If_inst_tR, Id_instr_tR, Ex_aluout_tR, DoneR, TR,
	       Id_InstrR1, Id_instr_tR1, Mem_aluout_tR1, DoneR1, TR1
	      ),
	  %% 
	  % -- both read same instructions
	  Cond1L=Cond1R,
	  %(If_InstrL=If_InstrR->Cond1L=Cond1R),
	  If_inst_tL1=0,
	  If_inst_tR1=0
/*
	;   % both done: spin.
	    DoneL=1, TL1=TL, DoneL1=1,
	    DoneR=1, TR1=TR, DoneR1=1
*/	
	),
	inv(
	    Id_InstrL,  If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, TL,
	    Id_InstrR,  If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR, TR
	   ).


DoneR=1:-
	inv(
	    Id_InstrL,  If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, TL,
	    Id_InstrR,  If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR, TR
	   ), DoneL=1.

