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

//
// Checker for bit packed multiply instruction with a pack width of 2
// - i.e. Two packed 16x16 bit carryless multiplies.
//
`VTX_CHECKER_MODULE_BEGIN(instr_pclmul_h_pw2)

// Pack width of the instruction
wire [2:0] pw = `VTX_INSTR_PACK_WIDTH;

// What we expect the result to be for a pclmul_h instruction.
reg [31:0] expected_result;

// Only check pclmul_h instructions
always @(posedge `VTX_CLK_NAME) if(vtx_valid) restrict(dec_pclmul_h);

//
// Include this file which contains a process computing "expected_result"
// Included so it can be shared across all "fml_chl_instr_pclmul_*" files.
`include "fml_pclmul_h_result.vh"

//
// pclmul_h
//
`VTX_CHECK_INSTR_BEGIN(pclmul_h) 
    
    `VTX_ASSUME(pw != SCARV_COP_PW_1  &&
                pw != SCARV_COP_PW_4  &&
                pw != SCARV_COP_PW_8  &&
                pw != SCARV_COP_PW_16 );

    // Correct pack width encoding value or instruction gives in bad
    // opcode result.
    `VTX_ASSERT_PACK_WIDTH_CORRECT

    // Result comes from the PACK_WIDTH_ARITH_OPERATION_RESULT macro.
    if(vtx_instr_result == SCARV_COP_INSN_SUCCESS) begin
        `VTX_ASSERT_CRD_VALUE_IS(expected_result)
        
        // Other pack widths covered by other proof files.
        `VTX_COVER(pw == SCARV_COP_PW_2 );
    end

    // Never causes writeback to GPRS
    `VTX_ASSERT_WEN_IS_CLEAR

`VTX_CHECK_INSTR_END(pclmul_h)

`VTX_CHECKER_MODULE_END


