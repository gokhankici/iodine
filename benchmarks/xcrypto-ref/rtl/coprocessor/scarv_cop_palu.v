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
// module: scarv_cop_palu
//
//  Combinatorial Packed arithmetic and shift module.
//
// notes:
//  - LMIX/HMIX expect crd value to be in palu_rs3
//  - LUI/LLI expect crd value to be in palu_rs3
//  - INS expects crd value to be in palu_rs3
//

`include "scarv_cop_palu_adder.v"
`include "scarv_cop_palu_shifter.v"
`include "scarv_cop_palu_multiplier.v"

module scarv_cop_palu (
input  wire         g_clk            ,
input  wire         g_resetn         ,

input  wire         palu_ivalid      , // Valid instruction input
output wire         palu_idone       , // Instruction complete

input  wire [31:0]  gpr_rs1          , // GPR Source register 1
input  wire [31:0]  palu_rs1         , // Source register 1
input  wire [31:0]  palu_rs2         , // Source register 2
input  wire [31:0]  palu_rs3         , // Source register 3

input  wire [31:0]  id_imm           , // Source immedate
input  wire [ 2:0]  id_pw            , // Pack width
input  wire [ 3:0]  id_class         , // Instruction class
input  wire [ 4:0]  id_subclass      , // Instruction subclass

output wire [ 3:0]  palu_cpr_rd_ben  , // Writeback byte enable
output wire [31:0]  palu_cpr_rd_wdata  // Writeback data
);

// @annot{taint_source(g_resetn)}
// @annot{taint_source(palu_ivalid)}
// @annot{taint_source(gpr_rs1)}
// @annot{taint_source(palu_rs1)}
// @annot{taint_source(palu_rs2)}
// @annot{taint_source(palu_rs3)}
// @annot{taint_source(id_imm)}
// @annot{taint_source(id_pw)}
// @annot{taint_source(id_class)}
// @annot{taint_source(id_subclass)}

// @annot{taint_sink(reg_palu_idone)}
// @annot{taint_sink(reg_palu_cpr_rd_ben)}
// @annot{taint_sink(reg_palu_cpr_rd_wdata)}

// -----------------------------------------------------------------------------
// manually found
// -----------------------------------------------------------------------------
// @annot-not{sanitize_mod(scarv_cop_palu_multiplier, ctr)}
// @annot-not{sanitize_glob(g_resetn)}
// @annot-not{sanitize_glob(id_class)}
// @annot-not{sanitize_glob(id_pw)}
// @annot-not{sanitize_glob(id_subclass)}
// @annot-not{sanitize_glob(palu_ivalid)}

// -----------------------------------------------------------------------------
// tool
// -----------------------------------------------------------------------------
// @annot{sanitize_glob(id_pw)}
// @annot{sanitize_glob(mul_start)}
// @annot{sanitize_glob(g_resetn)}
// @annot{sanitize(m_i_palu_multiplier_ctr)}


// REWRITE : extra assignments to check ct
reg reg_palu_idone;
reg [3:0] reg_palu_cpr_rd_ben;
reg [31:0] reg_palu_cpr_rd_wdata;
always @(*) begin
    reg_palu_idone = palu_idone;
    reg_palu_cpr_rd_ben = palu_cpr_rd_ben;
    reg_palu_cpr_rd_wdata = palu_cpr_rd_wdata;
end

// Commom field name and values.
`include "scarv_cop_common.vh"

// Purely combinatoral block.
assign palu_idone = palu_ivalid &&
                    (is_mul ? mul_done : 1'b1);

// Detect which subclass of instruction to execute.
wire is_mov_insn  = 
    palu_ivalid && id_class == SCARV_COP_ICLASS_MOVE;

wire is_bitwise_insn = 
    palu_ivalid && id_class == SCARV_COP_ICLASS_BITWISE;

wire is_parith_insn = 
    palu_ivalid && id_class == SCARV_COP_ICLASS_PACKED_ARITH;

wire is_twid_insn = 
    palu_ivalid && id_class == SCARV_COP_ICLASS_TWIDDLE;

//
// Result data muxing
assign palu_cpr_rd_wdata = 
    {32{is_mov_insn     }} & result_cmov    |
    {32{is_bitwise_insn }} & result_bitwise |
    {32{is_parith_insn  }} & result_parith  |
    {32{is_twid_insn    }} & result_twid    ;

//
// Should the result be written back?
assign palu_cpr_rd_ben = {4{palu_idone}} & (
    is_mov_insn                                       ? {4{wen_cmov}} :
    is_bitwise_insn || is_parith_insn || is_twid_insn ? 4'hF          :
                                                        4'h0         );

// ----------------------------------------------------------------------

//
//  Conditional Move Instructions
//

wire        cmov_cond   = palu_rs2 != 0;
wire [31:0] result_cmov = is_gpr2xcr ? gpr_rs1 : palu_rs1;

wire  is_cmov_t = is_mov_insn && id_subclass == SCARV_COP_SCLASS_CMOV_T  ;
wire  is_cmov_f = is_mov_insn && id_subclass == SCARV_COP_SCLASS_CMOV_F  ;
wire  is_gpr2xcr = is_mov_insn && id_subclass == SCARV_COP_SCLASS_GPR2XCR;

wire        wen_cmov    = 
        (is_gpr2xcr             ) ||
        (is_cmov_t &&  cmov_cond) ||
        (is_cmov_f && !cmov_cond)  ;

// ----------------------------------------------------------------------

//
//  Bitwise Instructions
//

wire bw_mix_l = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_MIX_L;
wire bw_mix_h = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_MIX_H;
wire bw_bop  = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_BOP ;
wire bw_ins  = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_INS ; 
wire bw_ext  = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_EXT ;
wire bw_ld_liu = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_LD_LIU;
wire bw_ld_hiu = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_LD_HIU;
wire bw_lut   = is_bitwise_insn && id_subclass == SCARV_COP_SCLASS_LUT;

// Result computation for the BOP.cr instruction
wire [31:0] bop_result;
// REWRITE : uncomment
// genvar br;
// generate for (br = 0; br < 32; br = br + 1)
//     assign bop_result[br] = id_imm[
//         {palu_rs3[br], palu_rs1[br], palu_rs2[br]}
//     ];
// endgenerate

// -----------------------------------------------------------------------------
// REWRITE : the following bunch are generated from above
wire bop_result_0 = id_imm[
    {palu_rs3[0], palu_rs1[0], palu_rs2[0]}
];
wire bop_result_1 = id_imm[
    {palu_rs3[1], palu_rs1[1], palu_rs2[1]}
];
wire bop_result_2 = id_imm[
    {palu_rs3[2], palu_rs1[2], palu_rs2[2]}
];
wire bop_result_3 = id_imm[
    {palu_rs3[3], palu_rs1[3], palu_rs2[3]}
];
wire bop_result_4 = id_imm[
    {palu_rs3[4], palu_rs1[4], palu_rs2[4]}
];
wire bop_result_5 = id_imm[
    {palu_rs3[5], palu_rs1[5], palu_rs2[5]}
];
wire bop_result_6 = id_imm[
    {palu_rs3[6], palu_rs1[6], palu_rs2[6]}
];
wire bop_result_7 = id_imm[
    {palu_rs3[7], palu_rs1[7], palu_rs2[7]}
];
wire bop_result_8 = id_imm[
    {palu_rs3[8], palu_rs1[8], palu_rs2[8]}
];
wire bop_result_9 = id_imm[
    {palu_rs3[9], palu_rs1[9], palu_rs2[9]}
];
wire bop_result_10 = id_imm[
    {palu_rs3[10], palu_rs1[10], palu_rs2[10]}
];
wire bop_result_11 = id_imm[
    {palu_rs3[11], palu_rs1[11], palu_rs2[11]}
];
wire bop_result_12 = id_imm[
    {palu_rs3[12], palu_rs1[12], palu_rs2[12]}
];
wire bop_result_13 = id_imm[
    {palu_rs3[13], palu_rs1[13], palu_rs2[13]}
];
wire bop_result_14 = id_imm[
    {palu_rs3[14], palu_rs1[14], palu_rs2[14]}
];
wire bop_result_15 = id_imm[
    {palu_rs3[15], palu_rs1[15], palu_rs2[15]}
];
wire bop_result_16 = id_imm[
    {palu_rs3[16], palu_rs1[16], palu_rs2[16]}
];
wire bop_result_17 = id_imm[
    {palu_rs3[17], palu_rs1[17], palu_rs2[17]}
];
wire bop_result_18 = id_imm[
    {palu_rs3[18], palu_rs1[18], palu_rs2[18]}
];
wire bop_result_19 = id_imm[
    {palu_rs3[19], palu_rs1[19], palu_rs2[19]}
];
wire bop_result_20 = id_imm[
    {palu_rs3[20], palu_rs1[20], palu_rs2[20]}
];
wire bop_result_21 = id_imm[
    {palu_rs3[21], palu_rs1[21], palu_rs2[21]}
];
wire bop_result_22 = id_imm[
    {palu_rs3[22], palu_rs1[22], palu_rs2[22]}
];
wire bop_result_23 = id_imm[
    {palu_rs3[23], palu_rs1[23], palu_rs2[23]}
];
wire bop_result_24 = id_imm[
    {palu_rs3[24], palu_rs1[24], palu_rs2[24]}
];
wire bop_result_25 = id_imm[
    {palu_rs3[25], palu_rs1[25], palu_rs2[25]}
];
wire bop_result_26 = id_imm[
    {palu_rs3[26], palu_rs1[26], palu_rs2[26]}
];
wire bop_result_27 = id_imm[
    {palu_rs3[27], palu_rs1[27], palu_rs2[27]}
];
wire bop_result_28 = id_imm[
    {palu_rs3[28], palu_rs1[28], palu_rs2[28]}
];
wire bop_result_29 = id_imm[
    {palu_rs3[29], palu_rs1[29], palu_rs2[29]}
];
wire bop_result_30 = id_imm[
    {palu_rs3[30], palu_rs1[30], palu_rs2[30]}
];
wire bop_result_31 = id_imm[
    {palu_rs3[31], palu_rs1[31], palu_rs2[31]}
];
assign bop_result = {bop_result_31, bop_result_30, bop_result_29, bop_result_28, bop_result_27, bop_result_26, bop_result_25, bop_result_24, bop_result_23, bop_result_22, bop_result_21, bop_result_20, bop_result_19, bop_result_18, bop_result_17, bop_result_16, bop_result_15, bop_result_14, bop_result_13, bop_result_12, bop_result_11, bop_result_10, bop_result_9, bop_result_8, bop_result_7, bop_result_6, bop_result_5, bop_result_4, bop_result_3, bop_result_2, bop_result_1, bop_result_0};
// -----------------------------------------------------------------------------

// Result computation for EXT / INST instructions
wire [ 4:0] ei_start    = {id_imm[7:4],1'b0};
wire [ 4:0] ei_len      = {id_imm[3:0],1'b0};

wire [31:0] ext_result  =
    (palu_rs1 >> ei_start) & ~(32'hFFFF_FFFF << ei_len);

wire [31:0] ins_mask    = 32'hFFFF_FFFF >> (32-ei_len);

wire [31:0] ins_result  =
    ((palu_rs1 & ins_mask) << ei_start) | 
    (palu_rs3 & ~(ins_mask << ei_start));

// Result computation for the MIX instructions
wire [ 4:0] mix_ramt = {bw_mix_h,id_imm[3:0]};

wire [31:0] mix_t0   =
    (palu_rs1 >> mix_ramt) | (palu_rs1 << (32-mix_ramt));

wire [31:0] mix_result =
    (palu_rs2 & mix_t0) | (~palu_rs2 & palu_rs3);

// Result computation for the LUT instruction.

wire [63:0] lut_concat = {64{bw_lut}} & {palu_rs3, palu_rs2};
wire [ 3:0] lut_lut [15:0];
wire [31:0] lut_result;
// REWRITE : uncomment
// genvar s;
// generate for(s = 0; s < 16; s = s+ 1) begin
//     if(s < 8) begin
//         assign lut_result[4*s+3:4*s] = lut_lut[palu_rs1[4*s+3 : 4*s]];
//     end
//     assign lut_lut[s] = lut_concat[4*s+3: 4*s];
// end endgenerate

// -----------------------------------------------------------------------------
// REWRITE : the following bunch are generated from above
assign lut_result_0 = lut_lut[palu_rs1[3 : 0]];
assign lut_lut_0 = lut_concat[3: 0];
assign lut_result_1 = lut_lut[palu_rs1[7 : 4]];
assign lut_lut_1 = lut_concat[7: 4];
assign lut_result_2 = lut_lut[palu_rs1[11 : 8]];
assign lut_lut_2 = lut_concat[11: 8];
assign lut_result_3 = lut_lut[palu_rs1[15 : 12]];
assign lut_lut_3 = lut_concat[15: 12];
assign lut_result_4 = lut_lut[palu_rs1[19 : 16]];
assign lut_lut_4 = lut_concat[19: 16];
assign lut_result_5 = lut_lut[palu_rs1[23 : 20]];
assign lut_lut_5 = lut_concat[23: 20];
assign lut_result_6 = lut_lut[palu_rs1[27 : 24]];
assign lut_lut_6 = lut_concat[27: 24];
assign lut_result_7 = lut_lut[palu_rs1[31 : 28]];
assign lut_lut_7 = lut_concat[31: 28];
assign lut_lut_8 = lut_concat[35: 32];
assign lut_lut_9 = lut_concat[39: 36];
assign lut_lut_10 = lut_concat[43: 40];
assign lut_lut_11 = lut_concat[47: 44];
assign lut_lut_12 = lut_concat[51: 48];
assign lut_lut_13 = lut_concat[55: 52];
assign lut_lut_14 = lut_concat[59: 56];
assign lut_lut_15 = lut_concat[63: 60];
assign lut_result = {lut_result_7, lut_result_6, lut_result_5, lut_result_4, lut_result_3, lut_result_2, lut_result_1, lut_result_0};
assign lut_lut = {lut_lut_15, lut_lut_14, lut_lut_13, lut_lut_12, lut_lut_11, lut_lut_10, lut_lut_9, lut_lut_8, lut_lut_7, lut_lut_6, lut_lut_5, lut_lut_4, lut_lut_3, lut_lut_2, lut_lut_1, lut_lut_0};
// -----------------------------------------------------------------------------

// AND/ORing the various bitwise results together.
wire [31:0] result_bitwise = 
    {32{bw_ld_liu}} & {palu_rs3[31:16], id_imm[15:0]    } |
    {32{bw_ld_hiu}} & {id_imm[15:0]   , palu_rs3[15: 0] } |
    {32{bw_bop }} & {bop_result                       } |
    {32{bw_bop }} & {bop_result                       } |
    {32{bw_ext }} & {ext_result                       } |
    {32{bw_ins }} & {ins_result                       } |
    {32{bw_mix_l}} & {mix_result                       } |
    {32{bw_mix_h}} & {mix_result                       } |
    {32{bw_lut}} & {lut_result                     } ;

// ----------------------------------------------------------------------

//
//  Twiddle Instructions
//

wire pperm_w  = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_W ;
wire pperm_h0 = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_H0;
wire pperm_h1 = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_H1;
wire pperm_b0 = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_B0;
wire pperm_b1 = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_B1;
wire pperm_b2 = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_B2;
wire pperm_b3 = is_twid_insn && id_subclass == SCARV_COP_SCLASS_PPERM_B3;

// Input signals to the twiddle logic
wire [7:0] pperm_w_in  [3:0];
wire [3:0] twid_n_in  [3:0];
wire [1:0] twid_c_in  [3:0];

// Output signals from the twiddle logic
wire [31:0] pperm_w_out;
wire [15:0] twid_n_out;
wire [ 7:0] twid_c_out;

// Result signals for the writeback logic
wire [31:0] pperm_w_result;
wire [31:0] twid_n_result;
wire [31:0] twid_c_result;

// Twiddle select indexes from instruction immediate
wire [1:0] b0  = id_imm[7:6];
wire [1:0] b1  = id_imm[5:4];
wire [1:0] b2  = id_imm[3:2];
wire [1:0] b3  = id_imm[1:0];

// Input halfword to twid.nX
wire [15:0] twid_n_hw = pperm_h0 ? palu_rs1[15:0] : palu_rs1[31:16];

// Input byte to twid.cX
wire [ 7:0] twid_c_b  =
    {8{pperm_b3}} & palu_rs1[31:24] |
    {8{pperm_b2}} & palu_rs1[23:16] |
    {8{pperm_b1}} & palu_rs1[15: 8] |
    {8{pperm_b0}} & palu_rs1[ 7: 0] ;

// Twiddle byte input array
// REWRITE : desugaring
// assign pperm_w_in[3] = palu_rs1[31:24];
// assign pperm_w_in[2] = palu_rs1[23:16];
// assign pperm_w_in[1] = palu_rs1[15: 8];
// assign pperm_w_in[0] = palu_rs1[ 7: 0];
assign pperm_w_in = {
    palu_rs1[31:24],
    palu_rs1[23:16],
    palu_rs1[15: 8],
    palu_rs1[ 7: 0]
};

// Twiddle nibble input array
// REWRITE : desugaring
// assign twid_n_in[3] = twid_n_hw[15:12];
// assign twid_n_in[2] = twid_n_hw[11: 8];
// assign twid_n_in[1] = twid_n_hw[ 7: 4];
// assign twid_n_in[0] = twid_n_hw[ 3: 0];
assign twid_n_in = {
    twid_n_hw[15:12],
    twid_n_hw[11: 8],
    twid_n_hw[ 7: 4],
    twid_n_hw[ 3: 0]
};

// Twiddle crumb input array.
// REWRITE : desugaring
// assign twid_c_in[3] = twid_c_b[7:6];
// assign twid_c_in[2] = twid_c_b[5:4];
// assign twid_c_in[1] = twid_c_b[3:2];
// assign twid_c_in[0] = twid_c_b[1:0];
assign twid_c_in = {
    twid_c_b[7:6],
    twid_c_b[5:4],
    twid_c_b[3:2],
    twid_c_b[1:0]
};

// Output array gathering
assign pperm_w_out = 
    {pperm_w_in[b3], pperm_w_in[b2], pperm_w_in[b1], pperm_w_in[b0]};

assign twid_n_out =
    {twid_n_in[b3], twid_n_in[b2], twid_n_in[b1], twid_n_in[b0]};

assign twid_c_out =
    {twid_c_in[b3], twid_c_in[b2], twid_c_in[b1], twid_c_in[b0]};

// Result construction
assign pperm_w_result = 
    {32{pperm_w}} & pperm_w_out;

assign twid_n_result = 
    {32{pperm_h0}} & {palu_rs1[31:16], twid_n_out} |
    {32{pperm_h1}} & {twid_n_out, palu_rs1[15: 0]} ;

assign twid_c_result = 
{32{pperm_b0}} & {palu_rs1[31:24],palu_rs1[23:16],palu_rs1[15:8],twid_c_out} |
{32{pperm_b1}} & {palu_rs1[31:24],palu_rs1[23:16],twid_c_out, palu_rs1[7:0]} |
{32{pperm_b2}} & {palu_rs1[31:24],twid_c_out, palu_rs1[15:8], palu_rs1[7:0]} |
{32{pperm_b3}} & {twid_c_out, palu_rs1[23:16],palu_rs1[15:8], palu_rs1[7:0]} ;

wire [31:0] result_twid = 
    pperm_w_result | twid_n_result | twid_c_result;

// ----------------------------------------------------------------------

//
//  Packed Arithmetic Instructions
//


wire is_padd  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PADD;
wire is_psub  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PSUB;
wire is_pmul_l  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PMUL_L;
wire is_pmul_h  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PMUL_H;
wire is_pclmul_l= is_parith_insn && id_subclass == SCARV_COP_SCLASS_PCLMUL_L;
wire is_pclmul_h= is_parith_insn && id_subclass == SCARV_COP_SCLASS_PCLMUL_H;
wire is_psll  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PSLL;
wire is_psrl  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PSRL;
wire is_prot  = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PROT;
wire is_psll_i = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PSLL_I;
wire is_psrl_i = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PSRL_I;
wire is_prot_i = is_parith_insn && id_subclass == SCARV_COP_SCLASS_PROT_I;

wire [31:0] result_parith;
    
wire [31:0] padd_a ;  // LHS input
wire [31:0] padd_b ;  // RHS input
wire        padd_sub; // Do subtract instead of add.
wire        padd_ci;  // Carry in
wire [31:0] padd_c ;  // Result
wire        padd_co;  // Carry out

wire        mul_start ; // Trigger to start multiplying
wire        mul_done  ; // Signal multiplication has finished.
wire        mul_hi    ; // Want high part of result
wire        mul_ncarry; // Do carryless multiplication.
wire [31:0] mul_a     ; // LHS operand
wire [31:0] mul_b     ; // RHS operand
wire [31:0] mul_result; // Result of the multiplication.
    
wire [31:0] pshf_a    ; // LHS input
wire [ 5:0] pshf_shamt; // RHS input
wire        pshf_sl   ; // shift left / n shift right
wire        pshf_r    ; // rotate / n shift
wire [31:0] pshf_c    ; // Result

assign padd_a = palu_rs1;
assign padd_b = palu_rs2;

assign mul_a       = palu_rs1;
assign mul_b       = palu_rs2;
assign mul_hi      = is_pmul_h || is_pclmul_h;
assign mul_ncarry  = is_pclmul_l || is_pclmul_h;
assign mul_start   = is_mul && is_parith_insn;

wire   shift_imm    = is_psll_i || is_psrl_i || is_prot_i;
assign pshf_a       = palu_rs1;
assign pshf_shamt   = shift_imm ? {1'b0,id_imm[4:0]  } :
                                  {1'b0,palu_rs2[4:0]} ;
assign pshf_r       = is_prot || is_prot_i;
assign pshf_sl      = is_psll || is_psll_i;

assign padd_ci      = is_psub;
assign padd_sub     = is_psub;

wire is_mul     = is_pmul_l || is_pmul_h || is_pclmul_l || is_pclmul_h ;

wire is_shift   = is_psll   || is_psrl   || is_prot     || is_psll_i   ||
                  is_psrl_i || is_prot_i ;

assign result_parith =
    {32{is_mul  }} & mul_result   |
    {32{is_padd }} & padd_c       |
    {32{is_psub }} & padd_c       |
    {32{is_shift}} & pshf_c       ;


scarv_cop_palu_adder i_palu_adder(
.a  (padd_a ),  // LHS input
.b  (padd_b ),  // RHS input
.pw (id_pw  ),  // Current operation pack width
.sub(padd_sub), // Do subtract instead of add.
.ci (padd_ci),  // Carry in
.c  (padd_c ),  // Result
.co (padd_co)   // Carry out
);

scarv_cop_palu_shifter i_palu_shifter (
.a    (pshf_a    ), // LHS input
.shamt(pshf_shamt), // RHS input
.pw   (id_pw     ), // Current operation pack width
.sl   (pshf_sl   ), // shift left / n shift right
.r    (pshf_r    ), // rotate / n shift
.c    (pshf_c    )  // Result
);

scarv_cop_palu_multiplier i_palu_multiplier (
.g_clk   (g_clk     ), // Global clock.
.g_resetn(g_resetn  ), // Global synchronous active low reset
.start   (mul_start ), // Trigger to start multiplying
.done    (mul_done  ), // Signal multiplication has finished.
.a       (mul_a     ), // LHS operand
.b       (mul_b     ), // RHS operand
.pw      (id_pw     ), // Pack width.
.high    (mul_hi    ),
.ncarry  (mul_ncarry),
.result  (mul_result)  // Result of the multiplication.
);

endmodule
