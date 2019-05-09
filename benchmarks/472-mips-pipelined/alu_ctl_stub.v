module alu_ctl_stub(ALUOp, Funct, ALUOperation);
    input [1:0] ALUOp;          // @annot{taint_source(ALUOp)}
    input [5:0] Funct;          // @annot{taint_source(Funct)}
    output [2:0] ALUOperation;  // @annot{taint_sink(ALUOperation)}
    reg    [2:0] ALUOperation;

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
      ALUOperation = ALUOp + Funct;
   
      // case (ALUOp) 
      //   2'b00 :
      //     ALUOperation = ALU_add;
      //   2'b10 : 
      //     case (Funct) 
      //       F_add : ALUOperation = ALU_add;
      //       default ALUOperation = 3'bxxx;
      //     endcase
      //   default:
      //     ALUOperation = 3'bxxx;
      // endcase

endmodule

