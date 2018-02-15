% -*- mode: prolog -*-
% vim: set ft=prolog:
:- multifile     register/1, wire/1, module_inst/3, always/2, link/2, asn/2, taint_source/1, taint_sink/1.
:- dynamic       register/1, wire/1, module_inst/3, always/2, link/2, asn/2, taint_source/1, taint_sink/1.
:- discontiguous register/1, wire/1, module_inst/3, always/2, link/2, asn/2, taint_source/1, taint_sink/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module type      : stalling_cpu
% module hierarchy : <toplevel>

register(v_EX_ALUOp).
register(v_ID_instr).
register(v_IF_instr).
register(v_MEM_ALUOut).
register(v_Stall).
register(v_WB_ALUOut).
wire(v_clk).    %%% input port

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module type      : decide_stall
% module hierarchy : Dec
% port assignments :
%   o <- v_Stall
%   i <- v_IF_instr

% register(v_IF_instr).    %%% input port
% register(v_Stall).    %%% output port

always(event1(star),
       b_asn(v_Stall, v_IF_instr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% module type      : alu
% module hierarchy : EX_ALU
% port assignments :
%   o <- v_EX_ALUOp
%   i <- v_ID_instr

% register(v_ID_instr).    %%% input port
% register(v_EX_ALUOp).    %%% output port

always(event1(star),
       b_asn(v_EX_ALUOp, v_ID_instr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


always(event2(posedge,v_clk),
       ite(v_Stall,
           nb_asn(v_ID_instr, v_ID_instr),
           nb_asn(v_ID_instr, v_IF_instr))).
always(event2(posedge,v_clk),
       nb_asn(v_MEM_ALUOut, v_EX_ALUOp)).
always(event2(posedge,v_clk),
       nb_asn(v_WB_ALUOut, v_MEM_ALUOut)).

taint_source(v_IF_instr).
taint_sink(v_WB_ALUOut).
