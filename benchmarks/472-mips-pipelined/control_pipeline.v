//-----------------------------------------------------------------------------
// Title         : MIPS Single-Cycle Control Unit
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : control_single.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
//
// Created       : November 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   Control unit for the MIPS pipelined  processor implementation described
//   Section 6.3 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).
//
//   It implements the function specified in Figure 6.25 on p. 401 of COD3e.
//
//-----------------------------------------------------------------------------

module control_pipeline(opcode, RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, ALUOp, Jump);
    // @annot{taint_eq(opcode)}

	// @annot{taint_source(RegDst)}
	// @annot{taint_source(ALUSrc)}
	// @annot{taint_source(MemtoReg)}
	// @annot{taint_source(RegWrite)}
	// @annot{taint_source(MemRead)}
	// @annot{taint_source(MemWrite)}
	// @annot{taint_source(Branch)}
	// @annot{taint_source(Jump)}

	// @annot{taint_sink(RegDst)}
	// @annot{taint_sink(ALUSrc)}
	// @annot{taint_sink(MemtoReg)}
	// @annot{taint_sink(RegWrite)}
	// @annot{taint_sink(MemRead)}
	// @annot{taint_sink(MemWrite)}
	// @annot{taint_sink(Branch)}
	// @annot{taint_sink(Jump)}

    input [5:0] opcode;
    output RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, Jump;
    output [1:0] ALUOp;
    reg    RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, Jump;
    reg    [1:0] ALUOp;

    parameter R_FORMAT = 6'd0;
    parameter J = 6'd2;
    parameter LW = 6'd35;
    parameter SW = 6'd43;
    parameter BEQ = 6'd4;

    always @(*)
    begin
        case (opcode)
          R_FORMAT :
          begin
              RegDst=1'b1; ALUSrc=1'b0; MemtoReg=1'b0; RegWrite=1'b1; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b0; ALUOp = 2'b10; Jump=1'b0;
          end
          J :
          begin
              RegDst=1'bx; ALUSrc=1'bx; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b0; ALUOp = 2'b00; Jump=1'b1;
          end
          LW :
          begin
              RegDst=1'b0; ALUSrc=1'b1; MemtoReg=1'b1; RegWrite=1'b1; MemRead=1'b1;
              MemWrite=1'b0; Branch=1'b0; ALUOp = 2'b00; Jump=1'b0;
          end
          SW :
          begin
              RegDst=1'bx; ALUSrc=1'b1; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0;
              MemWrite=1'b1; Branch=1'b0; ALUOp = 2'b00; Jump=1'b0;
          end
          BEQ :
          begin
              RegDst=1'bx; ALUSrc=1'b0; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b1; ALUOp = 2'b01; Jump=1'b0;
          end
          default
          begin
              $display("control_single unimplemented opcode %d", opcode);
              RegDst=1'b0; ALUSrc=1'b0; MemtoReg=1'b0; RegWrite=1'b0; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b0; ALUOp = 2'b00; Jump=1'b0;
          end
        endcase
    end
endmodule
