//-----------------------------------------------------------------------------
// Title         : MIPS Single-Cycle Control Unit
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : control_single.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   Control unit for the MIPS "Single Cycle" processor implementation described
//   Section 5.4 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//   It implements the function specified in Figure 5.18 on p. 308 of COD3e.
//
//-----------------------------------------------------------------------------

module control_single(opcode, RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, BranchNE, ALUOp, Jump);
    // MODIFICATIONS HERE:
    // Added new control signals BranchNE and Jump
    input [5:0] opcode;
    output RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, BranchNE, Jump;
    output [1:0] ALUOp;
    reg    RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, BranchNE, Jump;
    reg    [1:0] ALUOp;

    // MODIFICATIONS HERE:
    // Added new opcodes
    parameter R_FORMAT = 6'd0;
    parameter LW = 6'd35;
    parameter SW = 6'd43;
    parameter BEQ = 6'd4;
    parameter BNE = 6'd5;
    parameter J = 6'd2;
    parameter ADDI = 6'd8;

    // MODIFICATIONS HERE:
    // Implement new opcodes (individually commented) and set new control signals
    // for existing opcodes.
    always @(opcode)
    begin
        case (opcode)
          R_FORMAT : 
          begin
              RegDst=1'b1; ALUSrc=1'b0; MemtoReg=1'b0; RegWrite=1'b1; MemRead=1'b0; 
              MemWrite=1'b0; Branch=1'b0; BranchNE=1'b0; ALUOp = 2'b10; Jump=1'b0;
          end
          LW :
          begin
              RegDst=1'b0; ALUSrc=1'b1; MemtoReg=1'b1; RegWrite=1'b1; MemRead=1'b1; 
              MemWrite=1'b0; Branch=1'b0; BranchNE=1'b0; ALUOp = 2'b00; Jump=1'b0;
          end
          SW :
          begin
              RegDst=1'bx; ALUSrc=1'b1; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0; 
              MemWrite=1'b1; Branch=1'b0; BranchNE=1'b0; ALUOp = 2'b00; Jump=1'b0;
          end
          BEQ :
          begin
              RegDst=1'bx; ALUSrc=1'b0; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0; 
              MemWrite=1'b0; Branch=1'b1; BranchNE=1'b0; ALUOp = 2'b01; Jump=1'b0;
          end
          BNE :
          begin
              // Set BranchNE, ALUOp = subtract, disable all writes
              RegDst=1'bx; ALUSrc=1'b0; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b0; BranchNE=1'b1; ALUOp = 2'b01; Jump=1'b0;
          end
          J :
          begin
              // Set Jump and disable all writes
              RegDst=1'bx; ALUSrc=1'bx; MemtoReg=1'bx; RegWrite=1'b0; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b0; BranchNE=1'b0; ALUOp = 2'b00; Jump=1'b1;
          end
          ADDI :
          begin
              // RegDst = 0 to write the specified register, ALUSrc = 1 to pass
              // the immediate value to the ALU, RegWrite is set, ALUOp = add
              RegDst=1'b0; ALUSrc=1'b1; MemtoReg=1'b0; RegWrite=1'b1; MemRead=1'b0;
              MemWrite=1'b0; Branch=1'b0; BranchNE=1'b0; ALUOp = 2'b00; Jump=1'b0;
          end

          default
          begin
              $display("control_single unimplemented opcode %d", opcode);
              RegDst=1'bx; ALUSrc=1'bx; MemtoReg=1'bx; RegWrite=1'bx; MemRead=1'bx;
              MemWrite=1'bx; Branch=1'bx; BranchNE=1'bx; ALUOp = 2'bxx; Jump=1'bx;
          end

        endcase
    end
endmodule

