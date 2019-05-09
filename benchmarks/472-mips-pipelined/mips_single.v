//-----------------------------------------------------------------------------
// Title         : MIPS Single-Cycle Processor
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : mips_single.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   "Single Cycle" implementation of the MIPS processor subset described in
//   Section 5.4 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//   It implements the equivalent of Figure 5.19 on page 309 of COD3e
//
//-----------------------------------------------------------------------------

module mips_single(clk, reset);
    input clk, reset;

    // instruction bus
    wire [31:0] instr;

    // break out important fields from instruction
    wire [5:0] opcode, funct;
    wire [4:0] rs, rt, rd, shamt;
    wire [15:0] immed;
    wire [31:0] extend_immed, b_offset, j_addr;
    wire [25:0] jumpoffset;

    assign opcode = instr[31:26];
    assign rs = instr[25:21];
    assign rt = instr[20:16];
    assign rd = instr[15:11];
    assign shamt = instr[10:6];
    assign funct = instr[5:0];
    assign immed = instr[15:0];
    assign jumpoffset = instr[25:0];

    // sign-extender
    assign extend_immed = { {16{immed[15]}}, immed };

    // branch offset shifter
    assign b_offset = extend_immed << 2;

    // datapath signals
    // MODIFICATIONS HERE:
    // Add a few new wires
    wire [4:0] rfile_wn;
    wire [31:0] rfile_rd1, rfile_rd2, rfile_wd, alu_b, alu_out, b_tgt, pc_next,
                pc_jmp, pc, pc_incr, br_add_out, dmem_rdata;


    // MODIFICATIONS HERE:
    // Calculate the jump address based on the 4 highest bits of PC and the
    // jump offset shifted left by 2.
    assign j_addr = (pc_incr & 32'hf0000000) | (jumpoffset << 2);

    // control signals

    // MODIFICATIONS HERE:
    // New control signals BranchNE and Jump
    wire RegWrite, Branch, BranchNE, PCSrc, RegDst, MemtoReg, MemRead, MemWrite, ALUSrc, Zero, Jump;
    wire [1:0] ALUOp;
    wire [2:0] Operation;

    // module instantiations

    reg32       PC(clk, reset, pc_next, pc);

    add32       PCADD(pc, 32'd4, pc_incr);

    add32       BRADD(pc_incr, b_offset, b_tgt);

    reg_file    RFILE(clk, RegWrite, rs, rt, rfile_wn, rfile_rd1, rfile_rd2, rfile_wd); 

    alu         ALU(Operation, rfile_rd1, alu_b, alu_out, Zero);

    rom32       IMEM(pc, instr);

    mem32       DMEM(clk, MemRead, MemWrite, alu_out, rfile_rd2, dmem_rdata);

    // MODIFICATIONS HERE:
    // Branch if (Branch and zero are set) or (BrancNE and zero is not set)
    or          BR_OR(PCSrc, (Branch & Zero), (BranchNE & ~Zero));

    mux2 #(5)   RFMUX(RegDst, rt, rd, rfile_wn);

    // MODIFICATIONS HERE:
    // A new wire to route the output of this mux to input 0 of JMPMUX
    mux2 #(32)  PCMUX(PCSrc, pc_incr, b_tgt, pc_jmp);

    mux2 #(32)  ALUMUX(ALUSrc, rfile_rd2, extend_immed, alu_b);

    mux2 #(32)  WRMUX(MemtoReg, alu_out, dmem_rdata, rfile_wd);

    // MODIFICATIONS HERE:
    // A new mux to select between the calculated jump address and the output
    // of PCMUX
    mux2 #(32)  JMPMUX(Jump, pc_jmp, j_addr, pc_next);


    // MODIFICATIONS HERE:
    // Hook up new control signals BranchNE and Jump to the control unit
    control_single CTL(.opcode(opcode), .RegDst(RegDst), .ALUSrc(ALUSrc), .MemtoReg(MemtoReg), 
                       .RegWrite(RegWrite), .MemRead(MemRead), .MemWrite(MemWrite), .Branch(Branch), 
                       .BranchNE(BranchNE), .ALUOp(ALUOp), .Jump(Jump));

    alu_ctl   ALUCTL(ALUOp, funct, Operation);
endmodule
