%---------------------
%-- language:
%---------------------
%-- P ::=
%--    |  asn(x,y)       -- y<-x 
%--    |  link(x,y)      -- y<-fun(x) 
%--    |  link(x,y,z)    -- y<-fun(x,y) 
%--    |  cont(P)        -- continuous
%--    |  ite(exp,P,P)   -- conditional
%--    |  source(x)      -- taint source
%--    |  sink(x)        -- taint sink
%--  x,y in vars; c constant
%--  exp ::= 
%--    | x 
%--    | c
%--    | unop  exp
%--    | binop exp

%---------------------
%-- code:
%---------------------
%-- definitions:

%-- type "flag" indicates that not only taint bits but the
%-- *value*  should be tracked.

flag(stall).
flag(ex_memread).

register(if_inst).
register(id_inst).
register(ex_alu_op).
register(ex_extend).
register(ex_rt).
register(id_rs).
register(id_rt).

wire(id_op).
wire(id_aluop).
wire(id_immed).
wire(id_extend).
wire(ex_funct).
wire(ex_operation).
wire(ex_aluout).

wire(id_memread).
wire(id_memread_v).

% auxilliary
wire(cond1).

%-- if stage
ite( stall=1,
     asn(id_inst, id_inst),
     asn(if_inst, id_inst)
   ).

%-- hazard detection
%-- 
%-- paths:
%-- id_rs,id_rt, ex_memread  : id_instr
%-- ex_rt: id_rt
%-- 

cont(
     link(
     [ex_memread,ex_rt,id_rs,id_rt],
     cond1	  
).
cont(
     ite(
	 cond1
	 ),
	 asn(const(1), stall),
	 asn(const(0), stall)
	)
    ).

%-- wiring
cont(link(id_instr, id_op)).

cont(
     ite(stall,
	 asn(const(1), id_memread),
	 asn(id_memread_v, id_memread)
	)
    ).

% -- control pipeline
cont(link(id_op, id_memread_v)).
cont(link(id_op, id_aluop)).


%-- dec stage
% -- wiring
cont(link(id_inst, id_immed)).
cont(link(id_immed, id_extend)).
cont(link(id_instr, id_rt)).
cont(link(id_instr, id_rs)).

asn(id_aluop, ex_aluop).
asn(id_extend, ex_extend).
asn(id_memread, ex_memread).
asn(id_rt, ex_rt).

%-- ex stage
cont(link(ex_aluop, ex_funct)).
cont(link(ex_aluop, ex_operation)).
cont(link(ex_operation, nondet(), ex_aluout)).

asn(ex_aluout, mem_aluout).

%-- tag source and sink
source(if_inst).
sink(mem_aluout).
