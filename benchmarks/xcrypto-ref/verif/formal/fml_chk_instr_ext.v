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

`include "fml_pack_widths.vh"

`VTX_CHECKER_MODULE_BEGIN(instr_ext)

wire [ 4:0] ext_begin = {dec_arg_cs,1'b0};
wire [ 4:0] ext_len   = {dec_arg_cl,1'b0};
wire [31:0] ext_mask  = 32'hFFFF_FFFF >> (32-ext_len);
wire [31:0] ext_result= (`CRS1 >> ext_begin) & ext_mask;

//
// ext
//
`VTX_CHECK_INSTR_BEGIN(ext) 

    // Result comes from the PACK_WIDTH_ARITH_OPERATION_RESULT macro.
    `VTX_ASSERT_CRD_VALUE_IS(ext_result)

    // Never causes writeback to GPRS
    `VTX_ASSERT_WEN_IS_CLEAR

`VTX_CHECK_INSTR_END(ext)

`VTX_CHECKER_MODULE_END
