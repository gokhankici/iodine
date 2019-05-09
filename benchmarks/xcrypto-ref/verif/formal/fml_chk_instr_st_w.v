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

`include "fml_common.vh"

`VTX_CHECKER_MODULE_BEGIN(instr_st_w)

wire [31:0] sw_addr =
    vtx_instr_rs1 + 
    {{21{dec_arg_imm11hi[6]}},dec_arg_imm11hi,dec_arg_imm11lo};

//
// st_w
//
`VTX_CHECK_INSTR_BEGIN(st_w) 

    // Make sure it never gives the wrong error code.
    `VTX_ASSERT(vtx_instr_result != SCARV_COP_INSN_LD_ERR);
    `VTX_ASSERT(vtx_instr_result != SCARV_COP_INSN_BAD_LAD);

    if(sw_addr[1:0]) begin

        `VTX_ASSERT_RESULT_IS(SCARV_COP_INSN_BAD_SAD)

    end else if(vtx_instr_result == SCARV_COP_INSN_SUCCESS) begin
    
        // If the instruction succeeds, check it wrote the right data back

        `VTX_ASSERT(vtx_mem_cen_0  == 1'b1);
        `VTX_ASSERT(vtx_mem_wen_0  == 1'b1);
        `VTX_ASSERT(vtx_mem_ben_0  == 4'b1111);
        `VTX_ASSERT(vtx_mem_addr_0 == {sw_addr[31:2],2'b00});
        `VTX_ASSERT(vtx_mem_wdata_0 == `CRS2);

    end else if(vtx_instr_result == SCARV_COP_INSN_ST_ERR) begin
        
        // Transaction should have started correctly.
        `VTX_ASSERT(vtx_mem_cen_0  == 1'b1);
        `VTX_ASSERT(vtx_mem_wen_0  == 1'b1);
        `VTX_ASSERT(vtx_mem_ben_0  == 4'b1111);
        `VTX_ASSERT(vtx_mem_addr_0 == {sw_addr[31:2],2'b00});
        `VTX_ASSERT(vtx_mem_wdata_0 == `CRS2);

    end

    // Never causes writeback to GPRS
    `VTX_ASSERT_WEN_IS_CLEAR

`VTX_CHECK_INSTR_END(st_w)

`VTX_CHECKER_MODULE_END
