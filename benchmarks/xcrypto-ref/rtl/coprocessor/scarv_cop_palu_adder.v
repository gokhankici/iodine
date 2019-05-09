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
// module scarv_cop_palu_adder
//
//
module scarv_cop_palu_adder (
    input  wire [31:0] a ,  // LHS input
    input  wire [31:0] b ,  // RHS input
    input  wire [ 2:0] pw,  // Current operation pack width
    input  wire        sub, // Do subtract instead of add.
    input  wire        ci,  // Carry in
    output wire [31:0] c ,  // Result
    output wire        co   // Carry out
);

// @annot{taint_source(a)}
// @annot{taint_source(b)}
// @annot{taint_source(pw)}
// @annot{taint_source(sub)}
// @annot{taint_source(ci)}

// @annot{taint_sink(c_out)}
// @annot{taint_sink(co_out)}

// // REWRITE : added to check this module individually
// reg [31:0] c_out;
// reg co_out;
// always @(*) begin
//   co_out = co;
//   c_out  = c;
// end

`include "scarv_cop_common.vh"

// genvar i;

wire [31:0] fci        ; // Force carry in high - for subtract
wire [31:0] result_bits;
wire [32:0] carry_bits;
wire [32:0] carry_msk ;

// REWRITE
// assign carry_msk[32] = 1'b0;
// assign carry_bits[0] = ci;
wire carry_msk_32 = 1'b0;
wire carry_bits_0 = ci;

assign co            = carry_bits[32];
assign c             = result_bits[31:0];

wire   pw_1          = pw == SCARV_COP_PW_1 ; // 1  32-bit word
wire   pw_2          = pw == SCARV_COP_PW_2 ; // 2  16-bit halfwords
wire   pw_4          = pw == SCARV_COP_PW_4 ; // 4   8-bit bytes
wire   pw_8          = pw == SCARV_COP_PW_8 ; // 8   4-bit nibbles
wire   pw_16         = pw == SCARV_COP_PW_16; // 16  2-bit crumbs

wire [4:0]   pw_1hot       = {
    pw_1                                  , 
    pw_2                          || pw_1 ,
    pw_4                  || pw_2 || pw_1 ,
    pw_8          || pw_4 || pw_2 || pw_1 ,
    pw_16 || pw_8 || pw_4 || pw_2 || pw_1 
};

// REWRITE -- commented out
// generate for(i = 0; i < 32; i = i + 1) begin
//     wire [1:0] ir   = a[i]                             +
//                       (sub    ? !b[i] :  b[i]        ) +
//                       (fci[i] ? 1'b1  : carry_bits[i]) ;
//     assign fci[i] = i == 0 ? 1'b0   :
//                     sub && !carry_msk[i] && carry_msk[i-1];
//     assign carry_msk[i]     = (i & pw_1hot) != 0 || i == 0;
//     assign result_bits[i]   = ir[0];
//     assign carry_bits [i+1] = ir[1] && carry_msk[i+1];
// end endgenerate

// REWRITE
// the following is generated from the above block
wire [1:0] ir_0   = a[0] +
                      (sub ? !b[0] : b[0]) +
                      (fci_0 ? 1'b1  : carry_bits_0) ;
// NOTE: we applied constant prop to the next line
wire fci_0 = 1'b0;
wire carry_msk_0 = (0 & pw_1hot) != 0 || 0 == 0;
wire result_bits_0 = ir_0[0];
wire carry_bits_1 = ir_0[1] && carry_msk_1;

wire [1:0] ir_1   = a[1] +
                      (sub ? !b[1] : b[1]) +
                      (fci_1 ? 1'b1  : carry_bits_1) ;
wire fci_1 = 1 == 0 ? 1'b0 : sub && !carry_msk_1 && carry_msk_0;
wire carry_msk_1 = (1 & pw_1hot) != 0 || 1 == 0;
wire result_bits_1 = ir_1[0];
wire carry_bits_2 = ir_1[1] && carry_msk_2;

wire [1:0] ir_2   = a[2] +
                      (sub ? !b[2] : b[2]) +
                      (fci_2 ? 1'b1  : carry_bits_2) ;
wire fci_2 = 2 == 0 ? 1'b0 : sub && !carry_msk_2 && carry_msk_1;
wire carry_msk_2 = (2 & pw_1hot) != 0 || 2 == 0;
wire result_bits_2 = ir_2[0];
wire carry_bits_3 = ir_2[1] && carry_msk_3;

wire [1:0] ir_3   = a[3] +
                      (sub ? !b[3] : b[3]) +
                      (fci_3 ? 1'b1  : carry_bits_3) ;
wire fci_3 = 3 == 0 ? 1'b0 : sub && !carry_msk_3 && carry_msk_2;
wire carry_msk_3 = (3 & pw_1hot) != 0 || 3 == 0;
wire result_bits_3 = ir_3[0];
wire carry_bits_4 = ir_3[1] && carry_msk_4;

wire [1:0] ir_4   = a[4] +
                      (sub ? !b[4] : b[4]) +
                      (fci_4 ? 1'b1  : carry_bits_4) ;
wire fci_4 = 4 == 0 ? 1'b0 : sub && !carry_msk_4 && carry_msk_3;
wire carry_msk_4 = (4 & pw_1hot) != 0 || 4 == 0;
wire result_bits_4 = ir_4[0];
wire carry_bits_5 = ir_4[1] && carry_msk_5;

wire [1:0] ir_5   = a[5] +
                      (sub ? !b[5] : b[5]) +
                      (fci_5 ? 1'b1  : carry_bits_5) ;
wire fci_5 = 5 == 0 ? 1'b0 : sub && !carry_msk_5 && carry_msk_4;
wire carry_msk_5 = (5 & pw_1hot) != 0 || 5 == 0;
wire result_bits_5 = ir_5[0];
wire carry_bits_6 = ir_5[1] && carry_msk_6;

wire [1:0] ir_6   = a[6] +
                      (sub ? !b[6] : b[6]) +
                      (fci_6 ? 1'b1  : carry_bits_6) ;
wire fci_6 = 6 == 0 ? 1'b0 : sub && !carry_msk_6 && carry_msk_5;
wire carry_msk_6 = (6 & pw_1hot) != 0 || 6 == 0;
wire result_bits_6 = ir_6[0];
wire carry_bits_7 = ir_6[1] && carry_msk_7;

wire [1:0] ir_7   = a[7] +
                      (sub ? !b[7] : b[7]) +
                      (fci_7 ? 1'b1  : carry_bits_7) ;
wire fci_7 = 7 == 0 ? 1'b0 : sub && !carry_msk_7 && carry_msk_6;
wire carry_msk_7 = (7 & pw_1hot) != 0 || 7 == 0;
wire result_bits_7 = ir_7[0];
wire carry_bits_8 = ir_7[1] && carry_msk_8;

wire [1:0] ir_8   = a[8] +
                      (sub ? !b[8] : b[8]) +
                      (fci_8 ? 1'b1  : carry_bits_8) ;
wire fci_8 = 8 == 0 ? 1'b0 : sub && !carry_msk_8 && carry_msk_7;
wire carry_msk_8 = (8 & pw_1hot) != 0 || 8 == 0;
wire result_bits_8 = ir_8[0];
wire carry_bits_9 = ir_8[1] && carry_msk_9;

wire [1:0] ir_9   = a[9] +
                      (sub ? !b[9] : b[9]) +
                      (fci_9 ? 1'b1  : carry_bits_9) ;
wire fci_9 = 9 == 0 ? 1'b0 : sub && !carry_msk_9 && carry_msk_8;
wire carry_msk_9 = (9 & pw_1hot) != 0 || 9 == 0;
wire result_bits_9 = ir_9[0];
wire carry_bits_10 = ir_9[1] && carry_msk_10;

wire [1:0] ir_10   = a[10] +
                      (sub ? !b[10] : b[10]) +
                      (fci_10 ? 1'b1  : carry_bits_10) ;
wire fci_10 = 10 == 0 ? 1'b0 : sub && !carry_msk_10 && carry_msk_9;
wire carry_msk_10 = (10 & pw_1hot) != 0 || 10 == 0;
wire result_bits_10 = ir_10[0];
wire carry_bits_11 = ir_10[1] && carry_msk_11;

wire [1:0] ir_11   = a[11] +
                      (sub ? !b[11] : b[11]) +
                      (fci_11 ? 1'b1  : carry_bits_11) ;
wire fci_11 = 11 == 0 ? 1'b0 : sub && !carry_msk_11 && carry_msk_10;
wire carry_msk_11 = (11 & pw_1hot) != 0 || 11 == 0;
wire result_bits_11 = ir_11[0];
wire carry_bits_12 = ir_11[1] && carry_msk_12;

wire [1:0] ir_12   = a[12] +
                      (sub ? !b[12] : b[12]) +
                      (fci_12 ? 1'b1  : carry_bits_12) ;
wire fci_12 = 12 == 0 ? 1'b0 : sub && !carry_msk_12 && carry_msk_11;
wire carry_msk_12 = (12 & pw_1hot) != 0 || 12 == 0;
wire result_bits_12 = ir_12[0];
wire carry_bits_13 = ir_12[1] && carry_msk_13;

wire [1:0] ir_13   = a[13] +
                      (sub ? !b[13] : b[13]) +
                      (fci_13 ? 1'b1  : carry_bits_13) ;
wire fci_13 = 13 == 0 ? 1'b0 : sub && !carry_msk_13 && carry_msk_12;
wire carry_msk_13 = (13 & pw_1hot) != 0 || 13 == 0;
wire result_bits_13 = ir_13[0];
wire carry_bits_14 = ir_13[1] && carry_msk_14;

wire [1:0] ir_14   = a[14] +
                      (sub ? !b[14] : b[14]) +
                      (fci_14 ? 1'b1  : carry_bits_14) ;
wire fci_14 = 14 == 0 ? 1'b0 : sub && !carry_msk_14 && carry_msk_13;
wire carry_msk_14 = (14 & pw_1hot) != 0 || 14 == 0;
wire result_bits_14 = ir_14[0];
wire carry_bits_15 = ir_14[1] && carry_msk_15;

wire [1:0] ir_15   = a[15] +
                      (sub ? !b[15] : b[15]) +
                      (fci_15 ? 1'b1  : carry_bits_15) ;
wire fci_15 = 15 == 0 ? 1'b0 : sub && !carry_msk_15 && carry_msk_14;
wire carry_msk_15 = (15 & pw_1hot) != 0 || 15 == 0;
wire result_bits_15 = ir_15[0];
wire carry_bits_16 = ir_15[1] && carry_msk_16;

wire [1:0] ir_16   = a[16] +
                      (sub ? !b[16] : b[16]) +
                      (fci_16 ? 1'b1  : carry_bits_16) ;
wire fci_16 = 16 == 0 ? 1'b0 : sub && !carry_msk_16 && carry_msk_15;
wire carry_msk_16 = (16 & pw_1hot) != 0 || 16 == 0;
wire result_bits_16 = ir_16[0];
wire carry_bits_17 = ir_16[1] && carry_msk_17;

wire [1:0] ir_17   = a[17] +
                      (sub ? !b[17] : b[17]) +
                      (fci_17 ? 1'b1  : carry_bits_17) ;
wire fci_17 = 17 == 0 ? 1'b0 : sub && !carry_msk_17 && carry_msk_16;
wire carry_msk_17 = (17 & pw_1hot) != 0 || 17 == 0;
wire result_bits_17 = ir_17[0];
wire carry_bits_18 = ir_17[1] && carry_msk_18;

wire [1:0] ir_18   = a[18] +
                      (sub ? !b[18] : b[18]) +
                      (fci_18 ? 1'b1  : carry_bits_18) ;
wire fci_18 = 18 == 0 ? 1'b0 : sub && !carry_msk_18 && carry_msk_17;
wire carry_msk_18 = (18 & pw_1hot) != 0 || 18 == 0;
wire result_bits_18 = ir_18[0];
wire carry_bits_19 = ir_18[1] && carry_msk_19;

wire [1:0] ir_19   = a[19] +
                      (sub ? !b[19] : b[19]) +
                      (fci_19 ? 1'b1  : carry_bits_19) ;
wire fci_19 = 19 == 0 ? 1'b0 : sub && !carry_msk_19 && carry_msk_18;
wire carry_msk_19 = (19 & pw_1hot) != 0 || 19 == 0;
wire result_bits_19 = ir_19[0];
wire carry_bits_20 = ir_19[1] && carry_msk_20;

wire [1:0] ir_20   = a[20] +
                      (sub ? !b[20] : b[20]) +
                      (fci_20 ? 1'b1  : carry_bits_20) ;
wire fci_20 = 20 == 0 ? 1'b0 : sub && !carry_msk_20 && carry_msk_19;
wire carry_msk_20 = (20 & pw_1hot) != 0 || 20 == 0;
wire result_bits_20 = ir_20[0];
wire carry_bits_21 = ir_20[1] && carry_msk_21;

wire [1:0] ir_21   = a[21] +
                      (sub ? !b[21] : b[21]) +
                      (fci_21 ? 1'b1  : carry_bits_21) ;
wire fci_21 = 21 == 0 ? 1'b0 : sub && !carry_msk_21 && carry_msk_20;
wire carry_msk_21 = (21 & pw_1hot) != 0 || 21 == 0;
wire result_bits_21 = ir_21[0];
wire carry_bits_22 = ir_21[1] && carry_msk_22;

wire [1:0] ir_22   = a[22] +
                      (sub ? !b[22] : b[22]) +
                      (fci_22 ? 1'b1  : carry_bits_22) ;
wire fci_22 = 22 == 0 ? 1'b0 : sub && !carry_msk_22 && carry_msk_21;
wire carry_msk_22 = (22 & pw_1hot) != 0 || 22 == 0;
wire result_bits_22 = ir_22[0];
wire carry_bits_23 = ir_22[1] && carry_msk_23;

wire [1:0] ir_23   = a[23] +
                      (sub ? !b[23] : b[23]) +
                      (fci_23 ? 1'b1  : carry_bits_23) ;
wire fci_23 = 23 == 0 ? 1'b0 : sub && !carry_msk_23 && carry_msk_22;
wire carry_msk_23 = (23 & pw_1hot) != 0 || 23 == 0;
wire result_bits_23 = ir_23[0];
wire carry_bits_24 = ir_23[1] && carry_msk_24;

wire [1:0] ir_24   = a[24] +
                      (sub ? !b[24] : b[24]) +
                      (fci_24 ? 1'b1  : carry_bits_24) ;
wire fci_24 = 24 == 0 ? 1'b0 : sub && !carry_msk_24 && carry_msk_23;
wire carry_msk_24 = (24 & pw_1hot) != 0 || 24 == 0;
wire result_bits_24 = ir_24[0];
wire carry_bits_25 = ir_24[1] && carry_msk_25;

wire [1:0] ir_25   = a[25] +
                      (sub ? !b[25] : b[25]) +
                      (fci_25 ? 1'b1  : carry_bits_25) ;
wire fci_25 = 25 == 0 ? 1'b0 : sub && !carry_msk_25 && carry_msk_24;
wire carry_msk_25 = (25 & pw_1hot) != 0 || 25 == 0;
wire result_bits_25 = ir_25[0];
wire carry_bits_26 = ir_25[1] && carry_msk_26;

wire [1:0] ir_26   = a[26] +
                      (sub ? !b[26] : b[26]) +
                      (fci_26 ? 1'b1  : carry_bits_26) ;
wire fci_26 = 26 == 0 ? 1'b0 : sub && !carry_msk_26 && carry_msk_25;
wire carry_msk_26 = (26 & pw_1hot) != 0 || 26 == 0;
wire result_bits_26 = ir_26[0];
wire carry_bits_27 = ir_26[1] && carry_msk_27;

wire [1:0] ir_27   = a[27] +
                      (sub ? !b[27] : b[27]) +
                      (fci_27 ? 1'b1  : carry_bits_27) ;
wire fci_27 = 27 == 0 ? 1'b0 : sub && !carry_msk_27 && carry_msk_26;
wire carry_msk_27 = (27 & pw_1hot) != 0 || 27 == 0;
wire result_bits_27 = ir_27[0];
wire carry_bits_28 = ir_27[1] && carry_msk_28;

wire [1:0] ir_28   = a[28] +
                      (sub ? !b[28] : b[28]) +
                      (fci_28 ? 1'b1  : carry_bits_28) ;
wire fci_28 = 28 == 0 ? 1'b0 : sub && !carry_msk_28 && carry_msk_27;
wire carry_msk_28 = (28 & pw_1hot) != 0 || 28 == 0;
wire result_bits_28 = ir_28[0];
wire carry_bits_29 = ir_28[1] && carry_msk_29;

wire [1:0] ir_29   = a[29] +
                      (sub ? !b[29] : b[29]) +
                      (fci_29 ? 1'b1  : carry_bits_29) ;
wire fci_29 = 29 == 0 ? 1'b0 : sub && !carry_msk_29 && carry_msk_28;
wire carry_msk_29 = (29 & pw_1hot) != 0 || 29 == 0;
wire result_bits_29 = ir_29[0];
wire carry_bits_30 = ir_29[1] && carry_msk_30;

wire [1:0] ir_30   = a[30] +
                      (sub ? !b[30] : b[30]) +
                      (fci_30 ? 1'b1  : carry_bits_30) ;
wire fci_30 = 30 == 0 ? 1'b0 : sub && !carry_msk_30 && carry_msk_29;
wire carry_msk_30 = (30 & pw_1hot) != 0 || 30 == 0;
wire result_bits_30 = ir_30[0];
wire carry_bits_31 = ir_30[1] && carry_msk_31;

wire [1:0] ir_31   = a[31] +
                      (sub ? !b[31] : b[31]) +
                      (fci_31 ? 1'b1  : carry_bits_31) ;
wire fci_31 = 31 == 0 ? 1'b0 : sub && !carry_msk_31 && carry_msk_30;
wire carry_msk_31 = (31 & pw_1hot) != 0 || 31 == 0;
wire result_bits_31 = ir_31[0];
wire carry_bits_32 = ir_31[1] && carry_msk_32;

assign fci = {fci_31, fci_30, fci_29, fci_28, fci_27, fci_26, fci_25, fci_24, fci_23, fci_22, fci_21, fci_20, fci_19, fci_18, fci_17, fci_16, fci_15, fci_14, fci_13, fci_12, fci_11, fci_10, fci_9, fci_8, fci_7, fci_6, fci_5, fci_4, fci_3, fci_2, fci_1, fci_0};
assign result_bits = {result_bits_31, result_bits_30, result_bits_29, result_bits_28, result_bits_27, result_bits_26, result_bits_25, result_bits_24, result_bits_23, result_bits_22, result_bits_21, result_bits_20, result_bits_19, result_bits_18, result_bits_17, result_bits_16, result_bits_15, result_bits_14, result_bits_13, result_bits_12, result_bits_11, result_bits_10, result_bits_9, result_bits_8, result_bits_7, result_bits_6, result_bits_5, result_bits_4, result_bits_3, result_bits_2, result_bits_1, result_bits_0};
assign carry_bits = {carry_bits_32, carry_bits_31, carry_bits_30, carry_bits_29, carry_bits_28, carry_bits_27, carry_bits_26, carry_bits_25, carry_bits_24, carry_bits_23, carry_bits_22, carry_bits_21, carry_bits_20, carry_bits_19, carry_bits_18, carry_bits_17, carry_bits_16, carry_bits_15, carry_bits_14, carry_bits_13, carry_bits_12, carry_bits_11, carry_bits_10, carry_bits_9, carry_bits_8, carry_bits_7, carry_bits_6, carry_bits_5, carry_bits_4, carry_bits_3, carry_bits_2, carry_bits_1, carry_bits_0};
assign carry_msk = {carry_msk_32, carry_msk_31, carry_msk_30, carry_msk_29, carry_msk_28, carry_msk_27, carry_msk_26, carry_msk_25, carry_msk_24, carry_msk_23, carry_msk_22, carry_msk_21, carry_msk_20, carry_msk_19, carry_msk_18, carry_msk_17, carry_msk_16, carry_msk_15, carry_msk_14, carry_msk_13, carry_msk_12, carry_msk_11, carry_msk_10, carry_msk_9, carry_msk_8, carry_msk_7, carry_msk_6, carry_msk_5, carry_msk_4, carry_msk_3, carry_msk_2, carry_msk_1, carry_msk_0};

endmodule
