//
// SCARV Project
// 
// University of Bristol
// 
// RISC-V Cryptographic Instruction Set Extension
// 
// Reference Implementation
// 
// 

//
// module: scarv_cop_top
//
//  The top level module of the Crypto ISE co-processor.
//
module scarv_cop_top (

//
// Clock and reset interface

input  wire             g_clk           , // Global clock
output wire             g_clk_req       , // Clock request
input  wire             g_resetn        , // Synchronous active low reset.

//
// Status Interface
`ifdef FORMAL
output wire [31:0]      cop_random      , // The most recent random sample
output wire             cop_rand_sample , // cop_random valid when this high.

`VTX_REGISTER_PORTS_OUT(cprs_snoop)
`endif

//
// CPU / COP Interface
input  wire             cpu_insn_req    , // Instruction request
output                  cop_insn_ack    , // Instruction request acknowledge
input  wire [31:0]      cpu_insn_enc    , // Encoded instruction data
input  wire [31:0]      cpu_rs1         , // RS1 source data
input  wire [31:0]      cpu_rs2         , // RS2 source data

output                  cop_wen         , // COP write enable
output      [ 4:0]      cop_waddr       , // COP destination register address
output      [31:0]      cop_wdata       , // COP write data
output      [ 2:0]      cop_result      , // COP execution result
output                  cop_insn_rsp    , // COP instruction finished
input  wire             cpu_insn_ack    , // Instruction finish acknowledge

//
// Memory Interface
output wire             cop_mem_cen     , // Chip enable
output wire             cop_mem_wen     , // write enable
output wire [31:0]      cop_mem_addr    , // Read/write address (word aligned)
output wire [31:0]      cop_mem_wdata   , // Memory write data
input  wire [31:0]      cop_mem_rdata   , // Memory read data
output wire [ 3:0]      cop_mem_ben     , // Write Byte enable
input  wire             cop_mem_stall   , // Stall
input  wire             cop_mem_error     // Error

);

// Common constants & definitions
`include "scarv_cop_common.vh"

//
// If set, use the faster version of the COP/CPU interface where the
// COP assumes that:
// - `cpu_insn_ack` is always set.
// - `cpu_insn_enc` and `cpu_rs[1|2]` are stable until `cop_insn_rsp` is set
// - `cop_insn_rsp` and `cop_insn_ack` are equivilent.
//
// See implementation guide, section 2.3.2 for more information.
//
parameter FAST_COP_CPU_IF = 0;

//
// Glue logic wires
//

wire          id_exception    ; // Illegal instruction exception.

wire [ 3:0]   id_class        ; // Instruction class.
wire [ 4:0]   id_subclass     ; // Instruction subclass.

wire [ 2:0]   id_pw           ; // Instruction pack width.
wire [ 3:0]   id_crs1         ; // Instruction source register 1
wire [ 3:0]   id_crs2         ; // Instruction source register 2
wire [ 3:0]   id_crs3         ; // Instruction source register 3
wire [ 3:0]   id_crd          ; // Instruction destination register  
wire [ 3:0]   id_crd1         ; // MP Instruction destination register 1
wire [ 3:0]   id_crd2         ; // MP Instruction destination register 2
wire [ 4:0]   id_rd           ; // GPR destination register
wire [ 4:0]   id_rs1          ; // GPR source register
wire [31:0]   id_imm          ; // Decoded immediate.
wire          id_wb_h         ; // Halfword index (load/store)
wire          id_wb_b         ; // Byte index (load/store)

wire          crs1_ren   = 1'b1   ; // CPR Port 1 read enable
wire [ 3:0]   crs1_addr  = malu_rdm_in_rs ? id_crd1 : id_crs1; // CPR Port 1

wire          crs2_ren   = 1'b1   ; // CPR Port 2 read enable
wire [ 3:0]   crs2_addr  = malu_rdm_in_rs ? id_crd2 : id_crs2; // CPR Port 2

wire          crs3_ren   = 1'b1   ; // CPR Port 3 read enable
wire [ 3:0]   crs3_addr  = malu_rdm_in_rs ? id_crs1 : id_crs3; // CPR Port 3

wire [31:0]   crs1_rdata      ; // CPR Port 1 read data
wire [31:0]   crs2_rdata      ; // CPR Port 2 read data
wire [31:0]   crs3_rdata      ; // CPR Port 3 read data

wire [ 3:0]   crd_wen         ; // CPR Port 4 write enable
wire [ 3:0]   crd_addr        ; // CPR Port 4 address
wire [31:0]   crd_wdata       ; // CPR Port 4 write data

wire          insn_valid;
wire          insn_finish;
wire          insn_accept;
wire          fu_done;

wire          palu_ivalid      ; // Valid instruction input
wire          palu_idone       ; // Instruction complete
wire [ 3:0]   palu_cpr_rd_ben  ; // Writeback byte enable
wire [31:0]   palu_cpr_rd_wdata; // Writeback data

wire          mem_ivalid       ; // Valid instruction input
wire          mem_idone        ; // Instruction complete
wire          mem_is_store     ; // Instruction is a store / nload
wire          mem_addr_error   ; // Memory address exception
wire          mem_bus_error    ; // Memory bus exception
wire [ 3:0]   mem_cpr_rd_ben   ; // Writeback byte enable
wire [31:0]   mem_cpr_rd_wdata ; // Writeback data

wire          malu_rdm_in_rs   ; // Source destination registers in rs1/rs2
wire          malu_ivalid      ; // Valid instruction input
wire          malu_idone       ; // Instruction complete
wire [ 3:0]   malu_cpr_rd_ben  ; // Writeback byte enable
wire [31:0]   malu_cpr_rd_wdata; // Writeback data

wire          rng_ivalid       ; // Valid instruction input
wire          rng_idone        ; // Instruction complete
wire [ 3:0]   rng_cpr_rd_ben   ; // Writeback byte enable
wire [31:0]   rng_cpr_rd_wdata ; // Writeback data

wire          aes_ivalid       ; // Valid instruction input
wire          aes_idone        ; // Instruction complete
wire [ 3:0]   aes_cpr_rd_ben   ; // Writeback byte enable
wire [31:0]   aes_cpr_rd_wdata ; // Writeback data

wire          sha3_ivalid       ; // Valid instruction input
wire          sha3_idone        ; // Instruction complete
wire [ 3:0]   sha3_cpr_rd_ben   ; // Writeback byte enable
wire [31:0]   sha3_cpr_rd_wdata ; // Writeback data

//
// Functional unit dispatch
//
//  Send instructions to FU based on the decoded id_class.
//

assign palu_ivalid = 
    insn_valid  && (
    id_class == SCARV_COP_ICLASS_PACKED_ARITH ||
    id_class == SCARV_COP_ICLASS_MOVE         ||
    id_class == SCARV_COP_ICLASS_BITWISE      ||
    id_class == SCARV_COP_ICLASS_TWIDDLE      );

assign aes_ivalid =
    insn_valid  && (
    id_class == SCARV_COP_ICLASS_AES          );

assign sha3_ivalid =
    insn_valid  && (
    id_class == SCARV_COP_ICLASS_SHA3         );

assign malu_ivalid =
    insn_valid  && (
    id_class == SCARV_COP_ICLASS_MP           );

assign mem_ivalid  =
    insn_valid  && (
    id_class == SCARV_COP_ICLASS_LOADSTORE    );

assign rng_ivalid =
    insn_valid && (
    id_class == SCARV_COP_ICLASS_RANDOM       );

//
// CPR Writeback data selection
//
//  CPR writeback muxing from the functional units.
//

assign crd_wen   = palu_cpr_rd_ben |
                   mem_cpr_rd_ben  |
                   malu_cpr_rd_ben |
                   rng_cpr_rd_ben  |
                   aes_cpr_rd_ben  ;

assign crd_addr  = !malu_ivalid ? id_crd :
                   !malu_idone  ? id_crd1:
                                  id_crd2;

assign crd_wdata = palu_cpr_rd_wdata |
                   mem_cpr_rd_wdata  |
                   malu_cpr_rd_wdata |
                   rng_cpr_rd_wdata  |
                   aes_cpr_rd_wdata  ;

//
// GPR Writeback data and instruction result selection
//
//  Control writeback data for the GPRs, and the result of each
//  instruction.
//

wire        n_cop_wen   ; // COP write enable
wire [ 4:0] n_cop_waddr ; // COP destination register address
wire [31:0] n_cop_wdata ; // COP write data
wire [ 2:0] n_cop_result; // COP execution result

assign n_cop_waddr = id_rd;

assign n_cop_wen   = 
    (id_class     == SCARV_COP_ICLASS_MOVE    &&
     id_subclass  == SCARV_COP_SCLASS_XCR2GPR   )  ||
    (id_class     == SCARV_COP_ICLASS_SHA3      )  ||
    (id_class     == SCARV_COP_ICLASS_RANDOM  &&
     id_subclass  == SCARV_COP_SCLASS_RTEST     )  ||
    (id_class     == SCARV_COP_ICLASS_MP      &&
     (id_subclass == SCARV_COP_SCLASS_MEQU ||
      id_subclass == SCARV_COP_SCLASS_MLTE ||
      id_subclass == SCARV_COP_SCLASS_MGTE ) )   ;

assign n_cop_wdata = 
    id_class == SCARV_COP_ICLASS_MOVE   ? palu_cpr_rd_wdata : 
    id_class == SCARV_COP_ICLASS_SHA3   ? sha3_cpr_rd_wdata : 
    id_class == SCARV_COP_ICLASS_RANDOM ? rng_cpr_rd_wdata  : 
                                          malu_cpr_rd_wdata ;

//
//  and/or the result of the instruction together. Note
//  SCARV_COP_INSN_SUCCESS == 0
assign n_cop_result= 
    id_exception ?  SCARV_COP_INSN_BAD_INS : (
    {3{!mem_is_store & mem_addr_error}} & SCARV_COP_INSN_BAD_LAD |
    {3{!mem_is_store & mem_bus_error }} & SCARV_COP_INSN_LD_ERR  |
    {3{ mem_is_store & mem_addr_error}} & SCARV_COP_INSN_BAD_SAD |
    {3{ mem_is_store & mem_bus_error }} & SCARV_COP_INSN_ST_ERR  |
                                          SCARV_COP_INSN_SUCCESS );


wire [31:0] u_insn_enc;
wire [31:0] u_rs1;
wire [31:0] u_rs2;

//
// Start of generate statements to switch between the "fast" and
// "normal" interface protocols.
//

generate if(FAST_COP_CPU_IF == 0) begin

    reg            r_cop_wen   ; // COP write enable
    reg [ 4:0]     r_cop_waddr ; // COP destination register address
    reg [31:0]     r_cop_wdata ; // COP write data
    reg [ 2:0]     r_cop_result; // COP execution result

    //
    // COP Output response registers
    always @(posedge g_clk) if(!g_resetn) begin
        r_cop_wen    <= 1'b0; // COP write enable
        r_cop_waddr  <= 5'b0; // COP destination register address
        r_cop_wdata  <= 32'b0; // COP write data
        r_cop_result <= 3'b0; // COP execution result
    end else if(insn_finish) begin
        r_cop_wen    <= n_cop_wen   ; // COP write enable
        r_cop_waddr  <= n_cop_waddr ; // COP destination register address
        r_cop_wdata  <= n_cop_wdata ; // COP write data
        r_cop_result <= n_cop_result; // COP execution result
    end
    
    assign cop_wen    = r_cop_wen   ;
    assign cop_waddr  = r_cop_waddr ;
    assign cop_wdata  = r_cop_wdata ;
    assign cop_result = r_cop_result;

    //
    // Register inputs to the COP
    reg  [31:0] r_insn_enc;
    reg  [31:0] r_rs1;
    reg  [31:0] r_rs2;

    assign u_insn_enc = (insn_accept) ? cpu_insn_enc : r_insn_enc;
    assign u_rs1      = (insn_accept) ? cpu_rs1      : r_rs1     ;
    assign u_rs2      = (insn_accept) ? cpu_rs2      : r_rs2     ;

    always @(posedge g_clk) if(!g_resetn) begin
            r_insn_enc <= 32'b0;
        end else if(cpu_insn_req && cop_insn_ack)
            r_insn_enc <= cpu_insn_enc;    

    always @(posedge g_clk) if(!g_resetn) begin
            r_rs1      <= 32'b0;
        end else if(cpu_insn_req && cop_insn_ack)
            r_rs1      <= cpu_rs1     ;    
    
    always @(posedge g_clk) if(!g_resetn) begin
            r_rs2      <= 32'b0;
        end else if(cpu_insn_req && cop_insn_ack)
            r_rs2      <= cpu_rs2     ;    

end else if(FAST_COP_CPU_IF == 1) begin
        
    assign cop_wen    = n_cop_wen   ; // COP write enable
    assign cop_waddr  = n_cop_waddr ; // COP destination register address
    assign cop_wdata  = n_cop_wdata ; // COP write data
    assign cop_result = n_cop_result; // COP execution result

    assign u_insn_enc = cpu_insn_enc;
    assign u_rs1      = cpu_rs1     ;
    assign u_rs2      = cpu_rs2     ;

end endgenerate

//
// BEGIN PIPELINE PROGRESSION CONTROL

generate if(FAST_COP_CPU_IF == 0) begin

    assign  fu_done         = 
        mem_idone || palu_idone || malu_idone || 
        rng_idone || aes_idone  || sha3_idone ||
        (id_exception && insn_accept);
    
    assign  insn_valid      = insn_accept ||
                              cop_fsm == FSM_EXECUTING;
    assign  insn_accept     = cpu_insn_req && cop_insn_ack;
    wire    insn_retired    = cop_insn_rsp && cpu_insn_ack;
    assign  insn_finish     = fu_done;
    
    localparam FSM_IDLE         = 0;
    localparam FSM_WAITING      = 1;
    localparam FSM_EXECUTING    = 2;
    localparam FSM_FINISHED     = 3;
    
    reg           n_cop_insn_ack;
    reg           n_cop_insn_rsp;
    
    reg     [2:0] cop_fsm;
    reg     [2:0] n_cop_fsm;
    
    always @(*) begin
        
        n_cop_fsm       = FSM_IDLE;
        n_cop_insn_ack  = 1'b0;
        n_cop_insn_rsp  = 1'b0;
        
        case(cop_fsm)
    
        FSM_IDLE     : begin    // 0
            n_cop_fsm       = FSM_WAITING;
            n_cop_insn_ack  = 1'b1;
        end
    
        FSM_WAITING  : begin    // 1
            n_cop_insn_ack = 1'b1;
            if(cpu_insn_req && n_cop_insn_ack) begin
                if(insn_finish) begin
                    n_cop_fsm   = FSM_FINISHED;
                    n_cop_insn_rsp = 1'b1;
                    n_cop_insn_ack = 1'b0;
                end else begin
                    n_cop_fsm   = FSM_EXECUTING;
                    n_cop_insn_ack = 1'b0;
                end
            end else begin
                n_cop_fsm       = FSM_WAITING;
            end
        end
        
        FSM_EXECUTING: begin    // 2
            if(insn_finish) begin
                n_cop_fsm       = FSM_FINISHED;
                n_cop_insn_rsp  = 1'b1;
            end else begin
                n_cop_fsm       = FSM_EXECUTING;
            end
        end
    
        FSM_FINISHED : begin    // 3
            if(insn_retired) begin
                n_cop_fsm       = FSM_WAITING;
                n_cop_insn_ack  = 1'b1;
            end else begin
                n_cop_fsm       = FSM_FINISHED;
                n_cop_insn_rsp  = 1'b1;
            end
        end
    
    endcase end

    reg r_cop_insn_ack;
    reg r_cop_insn_rsp;

    assign cop_insn_ack = r_cop_insn_ack;
    assign cop_insn_rsp = r_cop_insn_rsp;
    
    always @(posedge g_clk) r_cop_insn_ack <= g_resetn ? n_cop_insn_ack : 1'b0;
    always @(posedge g_clk) r_cop_insn_rsp <= g_resetn ? n_cop_insn_rsp : 1'b0;
    
    always @(posedge g_clk) if(!g_resetn) begin
        cop_fsm <= FSM_IDLE;
    end else begin
        cop_fsm <= n_cop_fsm;
    end

end else if(FAST_COP_CPU_IF == 1) begin

    assign  fu_done         = 
        mem_idone || palu_idone || malu_idone ||
        rng_idone || aes_idone  || sha3_idone ||
        id_exception;

    assign  insn_valid      = cpu_insn_req;

    assign  cop_insn_rsp    = insn_valid && fu_done;
    assign  cop_insn_ack    = cop_insn_rsp;

end endgenerate


// END PIPELINE PROGRESSION CONTROL
//

// ----------------------------------------------------------------------

//
// Submodule Instantiations
//

//
// instance: scarv_cop_idecode
//
//  The instruction decoder for the ISE.
//
scarv_cop_idecode i_scarv_cop_idecode (
.id_encoded  (u_insn_enc  ), // Encoding 32-bit instruction
.id_exception(id_exception), // Illegal instruction exception.
.id_class    (id_class    ), // Instruction class.
.id_subclass (id_subclass ), // Instruction subclass.
.id_pw       (id_pw       ), // Instruction pack width.
.id_crs1     (id_crs1     ), // Instruction source register 1
.id_crs2     (id_crs2     ), // Instruction source register 2
.id_crs3     (id_crs3     ), // Instruction source register 3
.id_crd      (id_crd      ), // Instruction destination register
.id_crd1     (id_crd1     ), // MP Instruction destination register 1
.id_crd2     (id_crd2     ), // MP Instruction destination register 2
.id_rd       (id_rd       ), // GPR destination register
.id_rs1      (id_rs1      ), // GPR source register
.id_imm      (id_imm      ), // Decoded immediate.
.id_wb_h     (id_wb_h     ),
.id_wb_b     (id_wb_b     )
);


//
// instance: scarv_cop_cprs
//
//  The general purpose register file used by the COP.
//
scarv_cop_cprs i_scarv_cop_cprs(
.g_clk     (g_clk     ), // Global clock
.g_clk_req (g_clk_req ), // Clock request
.g_resetn  (g_resetn  ), // Synchronous active low reset.
`ifdef FORMAL
`VTX_REGISTER_PORTS_RAISE(cprs_snoop)
`endif
.crs1_ren  (crs1_ren  ), // Port 1 read enable
.crs1_addr (crs1_addr ), // Port 1 address
.crs1_rdata(crs1_rdata), // Port 1 read data
.crs2_ren  (crs2_ren  ), // Port 2 read enable
.crs2_addr (crs2_addr ), // Port 2 address
.crs2_rdata(crs2_rdata), // Port 2 read data
.crs3_ren  (crs3_ren  ), // Port 3 read enable
.crs3_addr (crs3_addr ), // Port 3 address
.crs3_rdata(crs3_rdata), // Port 3 read data
.crd_wen   (crd_wen   ), // Port 4 write enable
.crd_addr  (crd_addr  ), // Port 4 address
.crd_wdata (crd_wdata )  // Port 4 write data
);


//
// instance: scarv_cop_palu
//
//  Combinatorial Packed arithmetic and shift module.
//
// notes:
//  - LMIX/HMIX expect crd value to be in palu_rs3
//  - INS expects crd value to be in palu_rs3
//
scarv_cop_palu i_scarv_cop_palu (
.g_clk            (g_clk           ), // Global clock
.g_resetn         (g_resetn        ), // Synchronous active low reset.
.palu_ivalid      (palu_ivalid      ), // Valid instruction input
.palu_idone       (palu_idone       ), // Instruction complete
.gpr_rs1          (u_rs1            ), // GPR rs1
.palu_rs1         (crs1_rdata       ), // Source register 1
.palu_rs2         (crs2_rdata       ), // Source register 2
.palu_rs3         (crs3_rdata       ), // Source register 3
.id_imm           (id_imm           ), // Source immedate
.id_pw            (id_pw            ), // Pack width
.id_class         (id_class         ), // Instruction class
.id_subclass      (id_subclass      ), // Instruction subclass
.palu_cpr_rd_ben  (palu_cpr_rd_ben  ), // Writeback byte enable
.palu_cpr_rd_wdata(palu_cpr_rd_wdata)  // Writeback data
);


//
// instance: scarv_cop_aes
//
//  AES instruction implementations
//
//
scarv_cop_aes i_scarv_cop_aes(
.g_clk            (g_clk           ), // Global clock
.g_resetn         (g_resetn        ), // Synchronous active low reset.
.aes_ivalid       (aes_ivalid      ), // Valid instruction input
.aes_idone        (aes_idone       ), // Instruction complete
.aes_rs1          (crs1_rdata      ), // Source register 1
.aes_rs2          (crs2_rdata      ), // Source register 2
.id_class         (id_class        ), // Instruction class
.id_subclass      (id_subclass     ), // Instruction subclass
.aes_cpr_rd_ben   (aes_cpr_rd_ben  ), // Writeback byte enable
.aes_cpr_rd_wdata (aes_cpr_rd_wdata)  // Writeback data
);

//
// instance: scarv_cop_sha3
//
//  SHA3 instruction implementations
//
//
scarv_cop_sha3 i_scarv_cop_sha3 (
.g_clk            (g_clk           ), // Global clock
.g_resetn         (g_resetn        ), // Synchronous active low reset.
.sha3_ivalid      (sha3_ivalid     ), // Valid instruction input
.sha3_idone       (sha3_idone      ), // Instruction complete
.sha3_rs1         (u_rs1           ), // Source register 1
.sha3_rs2         (u_rs2           ), // Source register 2
.id_class         (id_class        ), // Instruction class
.id_subclass      (id_subclass     ), // Instruction subclass
.id_imm           (id_imm           ), // Source immedate
.sha3_cpr_rd_ben  (sha3_cpr_rd_ben  ), // Writeback byte enable
.sha3_cpr_rd_wdata(sha3_cpr_rd_wdata)  // Writeback data
);

//
// instance: scarv_cop_mem
//
//  Load/store memory access module.
//
scarv_cop_mem i_scarv_cop_mem (
.g_clk           (g_clk           ), // Global clock
.g_resetn        (g_resetn        ), // Synchronous active low reset.
.mem_ivalid      (mem_ivalid      ), // Valid instruction input
.mem_idone       (mem_idone       ), // Instruction complete
.mem_is_store    (mem_is_store    ), // Is the instruction a store?
.mem_addr_error  (mem_addr_error  ), // Memory address exception
.mem_bus_error   (mem_bus_error   ), // Memory bus exception
.gpr_rs1         (u_rs1           ), // Source register 1
.cpr_rs1         (crs1_rdata      ), // Source register 2
.cpr_rs2         (crs2_rdata      ), // Source register 3
.cpr_rs3         (crs3_rdata      ), // Source register 3
.id_wb_h         (id_wb_h         ), // Halfword index (load/store)
.id_wb_b         (id_wb_b         ), // Byte index (load/store)
.id_imm          (id_imm          ), // Source immedate
.id_class        (id_class        ), // Instruction class
.id_subclass     (id_subclass     ), // Instruction subclass
.mem_cpr_rd_ben  (mem_cpr_rd_ben  ), // Writeback byte enable
.mem_cpr_rd_wdata(mem_cpr_rd_wdata), // Writeback data
.cop_mem_cen     (cop_mem_cen     ), // Chip enable
.cop_mem_wen     (cop_mem_wen     ), // write enable
.cop_mem_addr    (cop_mem_addr    ), // Read/write address (word aligned)
.cop_mem_wdata   (cop_mem_wdata   ), // Memory write data
.cop_mem_rdata   (cop_mem_rdata   ), // Memory read data
.cop_mem_ben     (cop_mem_ben     ), // Write Byte enable
.cop_mem_stall   (cop_mem_stall   ), // Stall
.cop_mem_error   (cop_mem_error   )  // Error
);


//
// instance: scarv_cop_malu
//
//  Multi-precision arithmetic and shift module.
//
scarv_cop_malu i_scarv_cop_malu (
.g_clk            (g_clk           ), // Global clock
.g_resetn         (g_resetn        ), // Synchronous active low reset.
.malu_ivalid      (malu_ivalid      ), // Valid instruction input
.malu_idone       (malu_idone       ), // Instruction complete
.malu_rdm_in_rs   (malu_rdm_in_rs   ),
.gpr_rs1          (u_rs1            ),
.malu_rs1         (crs1_rdata       ), // Source register 1
.malu_rs2         (crs2_rdata       ), // Source register 2
.malu_rs3         (crs3_rdata       ), // Source register 3
.id_imm           (id_imm           ), // Source immedate
.id_class         (id_class         ), // Instruction class
.id_subclass      (id_subclass      ), // Instruction subclass
.malu_cpr_rd_ben  (malu_cpr_rd_ben  ), // Writeback byte enable
.malu_cpr_rd_wdata(malu_cpr_rd_wdata)  // Writeback data
);


scarv_cop_rng i_scarv_cop_rng(
.g_clk           (g_clk           ), // Global clock
.g_resetn        (g_resetn        ), // Synchronous active low reset.
.rng_ivalid      (rng_ivalid      ), // Valid instruction input
.rng_idone       (rng_idone       ), // Instruction complete
`ifdef FORMAL
.cop_random      (cop_random      ), // Latest random sample value
.cop_rand_sample (cop_rand_sample ), // random sample value valid
`endif
.rng_rs1         (crs1_rdata      ), // Source register 1
.id_imm          (id_imm          ), // Source immedate
.id_class        (id_class        ), // Instruction class
.id_subclass     (id_subclass     ), // Instruction subclass
.rng_cpr_rd_ben  (rng_cpr_rd_ben  ), // Writeback byte enable
.rng_cpr_rd_wdata(rng_cpr_rd_wdata) // Writeback data
);

endmodule
