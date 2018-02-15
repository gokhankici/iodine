// minimal yarvi carve-out
// no restart, interupts, bypass or other fancy things.
// only add/sub and and/or.

// DEFINITIONS
`define OP      12
`define OP_IMM  4
`define ADDSUB  0
`define OR      6
`define AND     7
`define opcode   [6 : 2]
`define funct3   [14:12]
`define rs1      [19:15]
`define rs2      [24:20]
`define INIT_PC  32'h00_0200

//registers
reg  [31:0] regs[0:31];

// 1. INSTRUCTION FETCH 
reg  [31:0] if_pc=0;
reg  [31:0] ex_next_pc = `INIT_PC;

// block Fetch: 
1: always @(posedge clock) begin
      if_pc <= ex_next_pc;
   end

// in the real implementation if_inst is wired to the actual address.
// m is just a made-up name for memory.
wire [31:0] if_inst=m[if_pc]; 

// 2. DECODE AND REGISTER FETCH 
reg  [31:0] de_inst;

// source and target registers
// in real implementation, there are some additional toggle bits   
reg  [31:0] de_rs1_val;
reg  [31:0] de_rs2_val;
   
// fetching registers
//block Regs: 
    always @(posedge clock) begin
1:   de_rs1_val <= regs[if_inst`rs1];
     de_rs2_val <= regs[if_inst`rs2];
    end

wire [31:0] de_rs2_val_imm  = de_rs2_val;
// next level, show that inst = add=> Property.   
//wire [31:0] de_rs2_val_imm  = de_inst`opcode == `OP_IMM ? de_i_imm : de_rs2_val;

//block Decode:   
   always @(posedge clock) begin
1:    de_inst   <= if_inst;
   end
   
// 3. EXECUTE
reg  [31:0] ex_res;
//block Exec 
always @(posedge clock) begin
1:      case (de_inst`opcode)
           `OP:
            case (de_inst`funct3)
               `ADDSUB: if (de_inst[30] && de_inst`opcode == `OP)
                           ex_res <= de_rs1_val - de_rs2_val_imm;
                        else
                           ex_res <= de_rs1_val + de_rs2_val_imm;
               `OR:   ex_res <= de_rs1_val | de_rs2_val_imm;
               `AND:  ex_res <= de_rs1_val & de_rs2_val_imm;
          endcase
       endcase  
end

// 4. WRITEBACK

// block ExecB
always @(posedge clock) 
     begin
1:      ex_inst <= de_inst;
     end
end
   
// In @(*) P, P is executed whenever one of the 
// registers occuring in P changes.

// block Result
   always @(*) 
1:    ex_wbv = ex_inst`opcode == `LOAD ? ex_ld_res : ex_res;
   end

// write back the result to register rd
// block Writeback
    always @(posedge clock) begin
1:     regs[ex_inst`rd] <= ex_wbv;
    end

// Update PC
// block PC
   always @(posedge clock) begin
1:    ex_next_pc <= ex_next_pc + 4;
   end
