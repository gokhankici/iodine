module control_pipeline_stub(opcode, RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, ALUOp, Jump);
   input [5:0]  opcode;
   output       RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, Jump;
   output [1:0] ALUOp;
   reg          RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, Jump;
   reg [1:0]    ALUOp;

   parameter R_FORMAT = 6'd0;
   parameter J        = 6'd2;
   parameter LW       = 6'd35;
   parameter SW       = 6'd43;
   parameter BEQ      = 6'd4;

    always @(*) begin
      case (opcode)
        R_FORMAT : begin
           ALUOp    = 2'b10;
           ALUSrc   = 1'b0;
           Branch   = 1'b0;
           Jump     = 1'b0;
           MemRead  = 1'b0;
           MemWrite = 1'b0;
           MemtoReg = 1'b0;
           RegDst   = 1'b1; 
           RegWrite = 1'b1;
        end
        default begin
           $display("control_single unimplemented opcode %d", opcode);
           ALUOp    = 2'b00;
           ALUSrc   = 1'b0;
           Branch   = 1'b0;
           Jump     = 1'b0;
           MemRead  = 1'b0;
           MemWrite = 1'b0;
           MemtoReg = 1'b0;
           RegDst   = 1'b0;
           RegWrite = 1'b0;
        end
      endcase
    end

   // always @(*) begin
   //    case (opcode)
   //      R_FORMAT : begin
   //         ALUOp    = 2'b10;
   //         ALUSrc   = 1'b0;
   //         Branch   = 1'b0;
   //         Jump     = 1'b0;
   //         MemRead  = 1'b0;
   //         MemWrite = 1'b0;
   //         MemtoReg = 1'b0;
   //         RegDst   = 1'b1; 
   //         RegWrite = 1'b1;
   //      end
   //      J : begin
   //         ALUOp    = 2'b00;
   //         ALUSrc   = 1'bx;
   //         Branch   = 1'b0;
   //         Jump     = 1'b1;
   //         MemRead  = 1'b0;
   //         MemWrite = 1'b0;
   //         MemtoReg = 1'bx;
   //         RegDst   = 1'bx;
   //         RegWrite = 1'b0;
   //      end
   //      LW : begin
   //         ALUOp    = 2'b00;
   //         ALUSrc   = 1'b1;
   //         Branch   = 1'b0;
   //         Jump     = 1'b0;
   //         MemRead  = 1'b1;
   //         MemWrite = 1'b0;
   //         MemtoReg = 1'b1;
   //         RegDst   = 1'b0;
   //         RegWrite = 1'b1;
   //      end
   //      SW : begin
   //         ALUOp    = 2'b00;
   //         ALUSrc   = 1'b1;
   //         Branch   = 1'b0;
   //         Jump     = 1'b0;
   //         MemRead  = 1'b0;
   //         MemWrite = 1'b1;
   //         MemtoReg = 1'bx;
   //         RegDst   = 1'bx;
   //         RegWrite = 1'b0;
   //      end
   //      BEQ : begin
   //         ALUOp    = 2'b01;
   //         ALUSrc   = 1'b0;
   //         Branch   = 1'b1;
   //         Jump     = 1'b0;
   //         MemRead  = 1'b0;
   //         MemWrite = 1'b0;
   //         MemtoReg = 1'bx;
   //         RegDst   = 1'bx;
   //         RegWrite = 1'b0;
   //      end
   //      default begin
   //         $display("control_single unimplemented opcode %d", opcode);
   //         ALUOp    = 2'b00;
   //         ALUSrc   = 1'b0;
   //         Branch   = 1'b0;
   //         Jump     = 1'b0;
   //         MemRead  = 1'b0;
   //         MemWrite = 1'b0;
   //         MemtoReg = 1'b0;
   //         RegDst   = 1'b0;
   //         RegWrite = 1'b0;
   //      end
   //    endcase
   // end

endmodule
