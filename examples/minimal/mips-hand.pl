% vim: set ft=prolog

query_naming(
	     inv(
		 if_instrl, id_instrl, id_rtl, ex_memreadl, ex_rtl, if_inst_tl, id_instr_tl, mem_aluout_tl, donel, 
		 if_instrr, id_instrr, id_rtr, ex_memreadr, ex_rtr, if_inst_tr, id_instr_tr, mem_aluout_tr, doner
		)
	    ).
% assignment of Y to X. (X->Y)

assign_op(X_T, Y_T) := X_T=Y_T. %ite(X_T>=1, Y_T>=1, Y_T=<0).

inv(
    If_InstrL ,Id_InstrL, Id_rtL, EX_MemReadL, Ex_rtL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, 
    If_InstrR, Id_InstrR, Id_rtR, EX_MemReadR, Ex_rtR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR
   ) :=
%If_InstrL=If_InstrR,
Id_InstrL=Id_InstrR,
Id_instr_tL=Id_instr_tR,
If_inst_tL=If_inst_tR,
Id_rtL=Id_rtR,
EX_MemReadL=EX_MemReadR,
Ex_rtL=Ex_rtR,             
If_inst_tL=If_inst_tR,
Mem_aluout_tL=Mem_aluout_tR, 
DoneL=DoneR.                   %-- not maintained

/*
inv(
    If_InstrL ,Id_InstrL, Id_rtL, EX_MemReadL, Ex_rtL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, 
    If_InstrR, Id_InstrR, Id_rtR, EX_MemReadR, Ex_rtR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR
   ) := 
ex_rtr=ex_rtl,ex_memreadr=ex_memreadl,id_rtr=id_rtl,id_instrr=id_instrl,if_instrr=if_instrl,
(
doner=1,id_instr_tr=1,if_inst_tr=0 ;
doner=1,id_instr_tr=0,if_inst_tr=0;
doner=0,id_instr_tr=1,if_inst_tr=0;
doner=0,id_instr_tr=0,if_inst_tr=1;
doner=0,id_instr_tr=0,if_inst_tr=0
)
).
*/


next(
     Stall, Cond1, If_Instr, Id_Instr, ID_MemRead, ID_MemRead_v, Id_rt, Ex_rt, If_inst_t, Id_instr_t, Ex_aluout_t, Done, 
     Id_Instr1, Id_instr_t1, Id_rt1, EX_MemRead1, Ex_rt1, Mem_aluout_t1, Done1
    ) := 
(
     Done=0,
     % wiring
     assign_op(Id_instr_t, Ex_aluout_t),
     %-- Hazard detection: Cond1 = (EX_MemRead && ((EX_rt == ID_rs) || (EX_rt == ID_rt))). 
     ite(Cond1=1, Stall=1, Stall=0),
     %
     ite(Stall=1, ID_MemRead=1, ID_MemRead=ID_MemRead_v),
     %
     % DEC stage
     (   Stall=1, assign_op(Id_instr_t, Id_instr_t1), Id_Instr1=Id_Instr
     ;   Stall=0, assign_op(If_inst_t, Id_instr_t1) , Id_Instr1=If_Instr
     ),
     %Ex stage
     Ex_rt1=Id_rt,
     EX_MemRead1=ID_MemRead,
     % WB stage
     assign_op(Ex_aluout_t, Mem_aluout_t1),
     ite(Mem_aluout_t1>=1, Done1=1, Done1=Done)
).

inv(
    If_InstrL ,Id_InstrL, Id_rtL, EX_MemReadL, Ex_rtL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, 
    If_InstrR, Id_InstrR, Id_rtR, EX_MemReadR, Ex_rtR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR
   ) :-
	If_inst_tL=1, Id_instr_tL=0, Mem_aluout_tL=0, DoneL=0, 
	If_inst_tR=1, Id_instr_tR=0, Mem_aluout_tR=0, DoneR=0, 
	%-- is this okay?
	Id_InstrL=Id_InstrR,
	If_InstrL=If_InstrR,
	%-- this assumes the pipeline state is equal, initially.
	%-- can we justify these? 
	Id_rtL=Id_rtR,
	Ex_rtL=Ex_rtR,
	EX_MemReadL=EX_MemReadR.


inv(
    If_InstrL1, Id_InstrL1, Id_rtL1, EX_MemReadL1, Ex_rtL1, If_inst_tL1, Id_instr_tL1, Mem_aluout_tL1, DoneL1, 
    If_InstrR1, Id_InstrR1, Id_rtR1, EX_MemReadR1, Ex_rtR1, If_inst_tR1, Id_instr_tR1, Mem_aluout_tR1, DoneR1
   ) :-
	(
	  % both executions have not finished yet: issue new a taint bit, register valuations.
	  DoneL=0,  DoneL1=0, 
	  DoneR=0, DoneR1=0, 
	  % all variable valuations stay the same.
	  Id_InstrL1=Id_InstrL, If_inst_tL1=1, Id_instr_tL1=0, Mem_aluout_tL1=0,
	  Id_InstrR1=Id_InstrR, If_inst_tR1=1, Id_instr_tR1=0, Mem_aluout_tR1=0,
	  If_InstrL1=If_InstrL, Id_rtL1=Id_rtL, Ex_rtL1=Ex_rtL,
	  If_InstrR1=If_InstrR, Id_rtR1=Id_rtR, Ex_rtR1=Ex_rtR,
	  EX_MemReadL1=EX_MemReadL,EX_MemReadR1=EX_MemReadR
	
	;  % both not done: both executions take a step.

	  next(
	       StallL, Cond1L, If_InstrL, Id_InstrL, ID_MemReadL, ID_MemRead_vL, Id_rtL, Ex_rtL, If_inst_tL, Id_instr_tL, Ex_aluout_tL, DoneL,
	       Id_InstrL1, Id_instr_tL1, Id_rtL1, EX_MemReadL1, Ex_rtL1, Mem_aluout_tL1, DoneL1
	      ),
	  next(
	       StallR, Cond1R, If_InstrR, Id_InstrR, ID_MemReadR, ID_MemRead_vR, Id_rtR, Ex_rtR, If_inst_tR, Id_instr_tR, Ex_aluout_tR, DoneR,
	       Id_InstrR1, Id_instr_tR1, Id_rtR1, EX_MemReadR1, Ex_rtR1, Mem_aluout_tR1, DoneR1
	      ),
	  %
	  %-- The valuation of both conditions is the same, if all occuring variables are equal.
	  ((Ex_rtL=Ex_rtR,Id_InstrL=Id_InstrR,EX_MemReadL=EX_MemReadR)->Cond1L=Cond1R),
	  % -- both read same instructions
	  If_InstrL=If_InstrR,  
	  If_InstrL1=If_InstrR1,
	  % -- functions
	  (Id_InstrL1=Id_InstrR1->(Id_rtL1=Id_rtR1,ID_MemRead_vL=ID_MemRead_vR)),
	  % -- new instruction doesn't have a tag
	  If_inst_tL1=0,
	  If_inst_tR1=0
	),
	inv(
	    If_InstrL, Id_InstrL, Id_rtL, EX_MemReadL, Ex_rtL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL, 
	    If_InstrR, Id_InstrR, Id_rtR, EX_MemReadR, Ex_rtR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR
	   ).


DoneL=1:-
	inv(
	    If_InstrL, Id_InstrL, Id_rtL, EX_MemReadL, Ex_rtL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL,
	    If_InstrR, Id_InstrR, Id_rtR, EX_MemReadR, Ex_rtR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR
	   ), DoneR=1.

DoneR=1:-
	inv(
	    If_InstrL, Id_InstrL, Id_rtL, EX_MemReadL, Ex_rtL, If_inst_tL, Id_instr_tL, Mem_aluout_tL, DoneL,
	    If_InstrR, Id_InstrR, Id_rtR, EX_MemReadR, Ex_rtR, If_inst_tR, Id_instr_tR, Mem_aluout_tR, DoneR
	   ), DoneL=1.

