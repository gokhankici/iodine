// simple stalling

module stalling_cpu(clk);
   input clk;
   
   //=============  
   // Definitions
   //=============

   // @annot{sanitize_glob(IF_instr)}
   // @annot{sanitize_glob(Stall)}

   reg [1:0]  Stall;
   wire  Stallw;
   reg [31:0] IF_instr;         // @annot{taint_source(IF_instr)}
   reg [31:0] ID_instr;
   reg [1:0]  EX_ALUOp;
   reg [31:0] MEM_ALUOut;
   reg [31:0] WB_ALUOut;        // @annot{taint_sink(WB_ALUOut)}.

   //=============  
   // IF stage
   //=============
   
   always @(posedge clk) 
     if (Stall)
       ID_instr <= ID_instr;
     else
       ID_instr <= IF_instr;

   // @annot{sanitize_mod(decide_stall, o)
   decide_stall Dec(IF_instr, Stallw);

   always @(posedge clk) 
     Stall <= Stallw;
   

   //=============
   // DEC stage
   //=============
   
   // no definition for this; treat as wire.
   wire       temp1;
   // @annot{sanitize_mod(alu, o)
   alu EX_ALU(ID_instr, temp1);
   always @(posedge clk)
     EX_ALUOp <= temp1;

   //=============
   // EXEC stage
   //=============

   always @(posedge clk)	
     MEM_ALUOut <= EX_ALUOp;

   //=============
   // MEM stage
   //=============

   always @(posedge clk)		
     WB_ALUOut <= MEM_ALUOut;

   //=============
   // WB stage
   //=============

endmodule

module decide_stall(i, o);
   input i;
   output o;
   reg    o;
   
   always @ (*)
     o = i;

endmodule

module alu(i, o);
   input  i;
   output o;
   reg    o;

   always @ (*)
     o = i;
endmodule
