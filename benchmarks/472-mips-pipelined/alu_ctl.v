//-----------------------------------------------------------------------------
// Title         : ALU Control Unit
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : alu_control.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   ALU Control Unit  used in the implementations of the MIPS
//   processor subset described in Ch. 5-6 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//   It implements the function specified in Figure 5.13 on p. 302 of COD3e.
//
//-----------------------------------------------------------------------------

module alu_ctl(ALUOp, Funct, ALUOperation);
   
   // @annot{sanitize_glob(ALUOp)}
   // @annot{sanitize_glob(Funct)}
   
   // @annot{taint_source(ALUOperation)}
   // @annot{taint_sink(ALUOperation)}

   
   input [1:0] ALUOp;
   input [5:0] Funct;
   output [2:0] ALUOperation;
   reg [2:0]    ALUOperation;

   // symbolic constants for instruction function code
   parameter F_add = 6'd32;
   parameter F_sub = 6'd34;
   parameter F_and = 6'd36;
   parameter F_or  = 6'd37;
   parameter F_slt = 6'd42;

   // symbolic constants for ALU Operations
   parameter ALU_add = 3'b010;
   parameter ALU_sub = 3'b110;
   parameter ALU_and = 3'b000;
   parameter ALU_or  = 3'b001;
   parameter ALU_slt = 3'b111;

   always @(*)
     begin
        case (ALUOp) 
          2'b00 : ALUOperation = ALU_add;
          2'b01 : ALUOperation = ALU_sub;
          2'b10 : case (Funct) 
                    F_add : ALUOperation = ALU_add;
                    F_sub : ALUOperation = ALU_sub;
                    F_and : ALUOperation = ALU_and;
                    F_or  : ALUOperation = ALU_or;
                    F_slt : ALUOperation = ALU_slt;
                    default ALUOperation = 3'bxxx;
                  endcase
          default ALUOperation = 3'bxxx;
        endcase
     end
endmodule

