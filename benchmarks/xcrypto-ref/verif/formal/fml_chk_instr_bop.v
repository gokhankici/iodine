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

`VTX_CHECKER_MODULE_BEGIN(instr_bop)

wire [31:0] bop_result;

genvar i;
generate for(i=0; i < 32; i = i + 1) begin
    
    wire [2:0] bop_index = {`CRD[i], `CRS1[i],`CRS2[i]};
    
    assign bop_result[i] = dec_arg_lut8[bop_index];

end endgenerate

//
// bop
//
`VTX_CHECK_INSTR_BEGIN(bop) 

    // Result comes from the PACK_WIDTH_ARITH_OPERATION_RESULT macro.
    `VTX_ASSERT_CRD_VALUE_IS(bop_result)

    // Never causes writeback to GPRS
    `VTX_ASSERT_WEN_IS_CLEAR

`VTX_CHECK_INSTR_END(bop)

`VTX_CHECKER_MODULE_END
