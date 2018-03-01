// simple stalling

module stalling_cpu(clk);
   input clk;
   
   //=============  
   // Definitions
   //=============

   // @annot{sanitize(IF_instr)}
   // @annot{sanitize(Stall)}

   reg [1:0]  Stall;
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

   // no definition for this; treat as wire.
   decide_stall Dec(IF_instr, Stall);

   //=============
   // DEC stage
   //=============
   
   // no definition for this; treat as wire.
   alu EX_ALU(ID_instr, EX_ALUOp);

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
   reg    i;
   reg    o;
   
   always @ (*)
     o = i;

endmodule

module alu(i, o);
   input i;
   output o;
   reg    i;
   reg    o;

   always @ (*)
     o = i;
endmodule
