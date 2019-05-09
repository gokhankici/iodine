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
register(ex_rt).
register(id_rs).
register(id_rt).


wire(ex_aluout).

wire(id_memread).
wire(id_memread_v).

%-- if stage
ite( stall=1,
     asn(id_inst, id_inst),
     asn(if_inst, id_inst)
   ).

%-- hazard detection
cont(
     ite(
	 (   ex_memread=1,
	     (   ex_rt==id_rs
	     ;   ex_rt==id_rt	     
	     )
	 ),
	 asn(const(1), stall),
	 asn(const(0), stall)
	)
    ).

%-- wiring

cont(
     ite(stall,
	 asn(const(1), id_memread),
	 asn(id_memread_v, id_memread)
	)
    ).

% -- control pipeline
cont(link(id_instr, id_memread_v)).


%-- dec stage
% -- wiring
cont(link(id_instr, id_rt)).
cont(link(id_instr, id_rs)).


asn(id_memread, ex_memread).
asn(id_rt, ex_rt).

%-- ex stage
cont(link(id_instr, nondet(), ex_aluout)).

asn(ex_aluout, mem_aluout).

asn(mem_aluout, wb_aluout).

%-- tag source and sink
source(if_inst).
sink(wb_aluout).
