//-----------------------------------------------------------------------------
// Title         : MIPS Pipelined Processor
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
//   Pipelined implementation of the MIPS processor subset described in
//   Section 6.3 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).
//
//   It implements the equivalent of Figure 6.27 on page 404 of COD3e
//
//-----------------------------------------------------------------------------

`include "reg32.v"
`include "mux2.v"
`include "mux3.v"
`include "control_pipeline.v" // `include "control_pipeline.v"
`include "alu.v"
`include "alu_ctl.v"
`include "add32.v"
`include "rom32.v"
`include "mem32.v"    // `include "mem32.v"
`include "reg_file.v" // `include "reg_file.v"

module mips_pipeline(clk, reset);
input clk, reset;
    // @annot{taint_source(ID_instr)}
    // @annot{taint_sink(EX_rt)}

    // @annot{sanitize_glob(reset)}

    // --------------------------------------------------------------------------------
    // VARS
    // --------------------------------------------------------------------------------

    // MODIFICATIONS HERE:
    // Add a new Stall signal
    reg Stall; // @annot{sanitize(Stall)}

    // ID Signal Declarations

    // @annot{sanitize(ID_instr)}
    reg [31:0] ID_instr;  // pipeline register values from EX

    wire [5:0] ID_op, ID_funct;
    wire [4:0] ID_rs, ID_rt, ID_rd;
    wire [15:0] ID_immed;
    wire [31:0] ID_extend, ID_rd1, ID_rd2;
    wire [31:0] ID_jaddr;

    // MODIFICATIONS HERE:
    // Intermediate control signals between the control unit and the stall
    // muxes. We only need to zero writes and branch/jumps, as well as memread
    // which could inadvertently trigger later stalls if its not zeroed.
    wire ID_RegWrite_v, ID_MemWrite_v, ID_MemRead_v, ID_Branch_v, ID_Jump_v;

    wire ID_RegWrite, ID_Branch, ID_RegDst, ID_MemtoReg,  // ID Control signals
         ID_MemRead, ID_MemWrite, ID_ALUSrc, ID_Jump;
    wire [1:0] ID_ALUOp;

    // EX Signals

    // MODIFICATIONS HERE:
    // Add EX_rs
    reg  [31:0] EX_pc4, EX_extend, EX_rd1, EX_rd2; // @annot{sanitize(EX_pc4, EX_extend, EX_rd1, EX_rd2)}
    wire [31:0]  EX_offset, EX_btgt, EX_alub, EX_ALUOut;
    reg  [4:0]  EX_rs, EX_rt, EX_rd; // @annot{sanitize(EX_rs, EX_rt, EX_rd)}
    wire [4:0]  EX_RegRd;
    wire [5:0] EX_funct;

    // @annot{sanitize(EX_RegWrite, EX_Branch, EX_RegDst, EX_MemtoReg, EX_MemRead, EX_MemWrite, EX_ALUSrc)}
    reg  EX_RegWrite, EX_Branch, EX_RegDst, EX_MemtoReg,  // EX Control Signals
         EX_MemRead, EX_MemWrite, EX_ALUSrc;

    wire EX_Zero;

    reg  [1:0] EX_ALUOp; // @annot{sanitize(EX_ALUOp)}
    wire [2:0] EX_Operation;

    // MODIFICATIONS HERE:
    // Add registers for forwarding control
    reg  [1:0] ForwardA, ForwardB; // @annot{sanitize(ForwardA, ForwardB)}

   // MEM Signals

    wire MEM_PCSrc;

    //@annot{sanitize(MEM_RegWrite, MEM_Branch, MEM_MemtoReg, MEM_MemRead, MEM_MemWrite, MEM_Zero)}
    reg  MEM_RegWrite, MEM_Branch, MEM_MemtoReg,
         MEM_MemRead, MEM_MemWrite, MEM_Zero;

    reg  [31:0] MEM_btgt, MEM_ALUOut, MEM_rd2; // @annot{sanitize(MEM_btgt, MEM_ALUOut, MEM_rd2)}
    wire [31:0] MEM_memout;
    reg  [5:0] MEM_RegRd; // @annot{sanitize(MEM_RegRd)}

    // WB Signals

    // @annot{sanitize(WB_RegWrite, WB_MemtoReg)}
    reg WB_RegWrite, WB_MemtoReg;  // WB Control Signals

    reg  [31:0] WB_memout, WB_ALUOut; // @annot{sanitize( WB_memout, WB_ALUOut )}
    wire [31:0] WB_wd;
    reg  [4:0] WB_RegRd; // @annot{sanitize(WB_RegRd)}

    // --------------------------------------------------------------------------------
    // LOGIC
    // --------------------------------------------------------------------------------

    assign ID_op = ID_instr[31:26];
    assign ID_rs = ID_instr[25:21];
    assign ID_rt = ID_instr[20:16];

    always @(posedge clk) begin
        if (reset) begin
          EX_rt <= 0;
        end else begin
          EX_rt <= ID_rt;
        end
    end

    // MODIFICATIONS HERE:
    // Connect ID_Jump to the control unit
    // @annot{sanitize_mod(control_pipeline, ALUOp)}
    // @annot{sanitize_mod(control_pipeline, ALUSrc)}
    // @annot{sanitize_mod(control_pipeline, Branch)}
    // @annot{sanitize_mod(control_pipeline, Jump)}
    // @annot{sanitize_mod(control_pipeline, MemRead)}
    // @annot{sanitize_mod(control_pipeline, MemWrite)}
    // @annot{sanitize_mod(control_pipeline, MemtoReg)}
    // @annot{sanitize_mod(control_pipeline, RegDst)}
    // @annot{sanitize_mod(control_pipeline, RegWrite)}
    control_pipeline CTL(.opcode(ID_op),           .RegDst(ID_RegDst),
                         .ALUSrc(ID_ALUSrc),       .MemtoReg(ID_MemtoReg),
                         .RegWrite(ID_RegWrite_v), .MemRead(ID_MemRead_v),
                         .MemWrite(ID_MemWrite_v), .Branch(ID_Branch_v),
                         .ALUOp(ID_ALUOp),         .Jump(ID_Jump_v));

    mux2 #(1)   ID_MR_SMUX(Stall, ID_MemRead_v,  1'b0, ID_MemRead);

    always @(posedge clk) begin
        if (reset) begin
          EX_MemRead  <= 0;
        end else begin
          EX_MemRead  <= ID_MemRead;
        end
    end

    always @(*)
    begin
        if (EX_MemRead
            && ((EX_rt == ID_rs) || (EX_rt == ID_rt)))
            Stall = 1'b1;
        else
            Stall = 1'b0;
    end

endmodule

