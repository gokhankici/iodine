// simplified version of the processor in  
// https://github.com/russellhaering/472-mips-pipelined/blob/master/mips_pipeline.v .
// Includes stalls (no reset).

`include "alu_stub.v"
`include "alu_ctl_stub.v"
`include "control_pipeline_stub.v"

module mips_pipeline(clk, reset);
   input clk, reset;

   // @annot{taint_source(IF_instr)}
   // @annot{taint_sink(ID_instr)}

   // @annot{sanitize_glob(IF_instr)}

   // @annot{sanitize(Stall, IF_instr, ID_instr, EX_ALUOp, EX_extend, EX_MemRead, MEM_ALUOut, WB_ALUOut, EX_rt)}

   //=============  
   // Definitions
   //=============

   reg [1:0] Stall;            
   reg [31:0] IF_instr;         
   reg [31:0] ID_instr;
   reg [1:0]  EX_ALUOp;
   reg [31:0] EX_extend;
   reg        EX_MemRead;

   reg [31:0] MEM_ALUOut;
   reg [31:0] WB_ALUOut;        
   reg [4:0]  EX_rt;
   
   wire [4:0] ID_rs, ID_rt;

   wire [5:0] ID_op;
   wire [1:0] ID_ALUOp;
   wire [15:0] ID_immed;
   wire [31:0] ID_extend;
   wire [5:0]  EX_funct;
   wire [2:0]  EX_Operation;
   wire [31:0] EX_ALUOut;
   wire        ID_MemRead, ID_MemRead_v;

   wire        ID_RegWrite_v, ID_MemWrite_v, ID_Branch_v, ID_Jump_v, ID_RegDst, ID_MemtoReg, ID_ALUSrc;
   //=============  
   // IF stage
   //=============

   // -- magic happens here
   always @(posedge clk) 
     if (Stall)
       ID_instr <= ID_instr;
     else
       ID_instr <= IF_instr;

   // Hazard detection
   always @(*) 
     if (EX_MemRead && ((EX_rt == ID_rs) || (EX_rt == ID_rt)))
       Stall = 1;
     else
       Stall = 0;

   // wiring
   assign ID_op      = ID_instr[31:26];
   assign ID_MemRead = Stall ? 1'b0 : ID_MemRead_v;

   //--------------------------------------
   // Map OP-code to internal control code.
   //--------------------------------------
   // Don't expand; treat as uninterpreted funciton of inputs.
   // This seems fine as output is fully determined by ID_op.
   // Easy to spot as modules declare inputs and outputs.

   // @annot{sanitize_mod(control_pipeline_stub, ALUOp)}
   // @annot{sanitize_mod(control_pipeline_stub, ALUSrc)}
   // @annot{sanitize_mod(control_pipeline_stub, Branch)}
   // @annot{sanitize_mod(control_pipeline_stub, Jump)}
   // @annot{sanitize_mod(control_pipeline_stub, MemRead)}
   // @annot{sanitize_mod(control_pipeline_stub, MemWrite)}
   // @annot{sanitize_mod(control_pipeline_stub, MemtoReg)}
   // @annot{sanitize_mod(control_pipeline_stub, RegDst)}
   // @annot{sanitize_mod(control_pipeline_stub, RegWrite)}
   control_pipeline_stub CTL
     (.opcode(ID_op), 
      .RegDst(ID_RegDst),
      .ALUSrc(ID_ALUSrc), 
      .MemtoReg(ID_MemtoReg),
      .RegWrite(ID_RegWrite_v), 
      .MemRead(ID_MemRead_v),
      .MemWrite(ID_MemWrite_v), 
      .Branch(ID_Branch_v),
      .ALUOp(ID_ALUOp), 
      .Jump(ID_Jump_v)
      );

   //=============
   // DEC stage
   //=============

   // treat all of those as uninterpreted functions of ID_instr.
   assign ID_immed  = ID_instr[15:0];
   assign ID_extend = { {16{ID_immed[15]}}, ID_immed };
   assign ID_rt     = ID_instr[20:16];
   assign ID_rs     = ID_instr[25:21];

   always @(posedge clk) // ID/EX Pipeline Register
     begin
        EX_ALUOp    <= ID_ALUOp;
        EX_extend   <= ID_extend;
        EX_MemRead  <= ID_MemRead;
        EX_rt       <= ID_rt;
     end

   //=============
   // EXEC stage
   //=============

   assign EX_funct = EX_extend[5:0];

   // Same as before: simplifying assumption.
   //
   // @annot{sanitize_mod(alu_ctl_stub, ALUOperation)}
   alu_ctl_stub EX_ALUCTL(EX_ALUOp, EX_funct, EX_Operation);


   wire zero;
   reg  a; // @annot{sanitize(a)}
   reg  b; // @annot{sanitize(b)}
   // @annot{sanitize_mod(alu_stub, result)}
   // @annot{sanitize_mod(alu_stub, zero)}
   alu_stub     EX_ALU(EX_Operation, a, b, EX_ALUOut, zero);

   always @(posedge clk)	
     MEM_ALUOut <= EX_ALUOut;

   //=============
   // MEM stage
   //=============

   always @(posedge clk)		
     WB_ALUOut <= MEM_ALUOut;

   //=============
   // WB stage
   //=============


endmodule
