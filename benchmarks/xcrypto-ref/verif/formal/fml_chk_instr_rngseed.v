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


`VTX_CHECKER_MODULE_BEGIN(instr_rngseed)

//
// rngseed
//
//  rngseed is a difficult one to verify without knowing or modeling
//  the randomness algorithm underneath it.
//
//  We settle here for making sure the simple side effects of the instruction
//  are kept too.
//
`VTX_CHECK_INSTR_BEGIN(rngseed) 
    
    // No change to CRD register vlaue.
    `VTX_ASSERT_CRD_VALUE_IS(`CRD)
    
    // Always succeeds
    `VTX_ASSERT_RESULT_IS(SCARV_COP_INSN_SUCCESS)

    // Never writes to GPRS
    `VTX_ASSERT_WEN_IS_CLEAR
    
`VTX_CHECK_INSTR_END(rngseed)

`VTX_CHECKER_MODULE_END
