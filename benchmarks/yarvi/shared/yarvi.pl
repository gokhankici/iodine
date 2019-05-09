% -*- mode: prolog -*-
% vim: set ft=prolog:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
topmodule(
[input(clock), output(address), output(writeenable), output(writedata), output(byteena), output(readenable), input(readdata), output(bus_req_ready), input(bus_req_read), input(bus_req_write), input(bus_req_address), input(bus_req_data), output(bus_res_valid), output(bus_res_data), input(bus_csr_no), input(bus_csr_read_enable), input(bus_csr_write_enable), input(bus_csr_writedata), output(bus_csr_readdata), output(bus_csr_readdata_valid)], 
[register(address), wire(b_addr), wire(b_din), wire(bus_csr_no), wire(bus_csr_read_enable), register(bus_csr_readdata), wire(bus_csr_readdata_valid), wire(bus_csr_write_enable), wire(bus_csr_writedata), wire(bus_req_address), wire(bus_req_data), wire(bus_req_read), wire(bus_req_read_go), wire(bus_req_ready), wire(bus_req_rw_go), wire(bus_req_write), wire(bus_req_write_go), register(bus_res_data), register(bus_res_valid), register(byteena), wire(clock), register(csr_cycle), register(csr_fflags), register(csr_frm), register(csr_instret), register(csr_mbadaddr), register(csr_mcause), register(csr_mcpuid), register(csr_mepc), register(csr_mfromhost), register(csr_mhartid), register(csr_mie), register(csr_mimpid), register(csr_mip), register(csr_mscratch), register(csr_mstatus), register(csr_mtdeleg), register(csr_mtime), register(csr_mtimecmp), register(csr_mtohost), register(csr_mtvec), register(csr_time), wire(de_branch_taken), wire(de_bytemask), wire(de_cmp_eq), wire(de_cmp_lt), register(de_csr_val), register(de_csrd), wire(de_i_imm), wire(de_illegal_csr_access), register(de_inst), wire(de_load), wire(de_load_addr), wire(de_load_byteena), wire(de_load_wa), register(de_pc), wire(de_rs1_forward_ex), wire(de_rs1_forward_wb), wire(de_rs1_val), wire(de_rs1_val_cmp), register(de_rs1_val_r), wire(de_rs2_forward_ex), wire(de_rs2_forward_wb), wire(de_rs2_val), wire(de_rs2_val_cmp), wire(de_rs2_val_imm), register(de_rs2_val_r), wire(de_rs2_val_shl), wire(de_s_imm), wire(de_sb_imm), wire(de_sign), wire(de_sign12), wire(de_sign20), wire(de_store), wire(de_store_addr), wire(de_store_byteena), wire(de_store_local), wire(de_store_wa), wire(de_uj_imm), wire(de_valid), register(de_valid_), register(ex_csr_res), register(ex_csrd), register(ex_inst), wire(ex_ld), register(ex_ld_res), register(ex_ld_shifted), register(ex_load_addr), register(ex_load_byteena), wire(ex_loaded_data), register(ex_loaded_data_is_bus_res), register(ex_next_pc), register(ex_res), register(ex_restart), wire(ex_valid), register(ex_valid_), register(ex_wben), register(ex_wbv), wire(if_inst), register(if_pc), wire(if_valid), register(if_valid_), wire(interrupt), register(interrupt_cause), wire(interrupt_mask), wire(readdata), register(readenable), register(regs), register(wb_csrd), register(wb_inst), register(wb_valid), register(wb_wben), register(wb_wbv), register(writedata), register(writeenable)],
[
asn(bus_req_read_go, uf_0_bus_req_read),
asn(bus_req_write_go, uf_1_bus_req_write),
asn(bus_req_rw_go, uf_2_bus_req_write_go),
asn(interrupt, uf_4_csr_mie),
asn(interrupt_mask, uf_5_csr_mip),
asn(ex_valid, ex_valid_),
asn(if_valid, uf_6_ex_restart),
asn(de_valid, uf_7_ex_restart),
asn(de_illegal_csr_access, uf_15_uf_14_de_inst),
asn(de_rs1_forward_ex, uf_18_uf_16_de_inst),
asn(de_rs2_forward_ex, uf_21_uf_19_de_inst),
asn(de_rs1_forward_wb, uf_24_wb_wben),
asn(de_rs2_forward_wb, uf_27_wb_wben),
asn(de_rs1_val, uf_28_de_rs1_val_r),
asn(de_rs2_val, uf_29_de_rs2_val_r),
asn(de_rs1_val_cmp, uf_31_de_rs1_val),
asn(de_rs2_val_cmp, uf_33_de_rs2_val),
asn(de_cmp_eq, uf_34_de_rs2_val),
asn(de_cmp_lt, uf_35_de_rs2_val_cmp),
asn(de_branch_taken, uf_38_de_cmp_eq),
asn(de_sign, uf_39_de_inst),
asn(de_sign20, uf_40_de_sign),
asn(de_sign12, uf_41_de_sign),
asn(de_i_imm, uf_44_uf_43_de_inst),
asn(de_s_imm, uf_47_uf_46_de_inst),
asn(de_sb_imm, uf_51_uf_50_de_inst),
asn(de_uj_imm, uf_55_uf_54_de_inst),
asn(de_rs2_val_imm, uf_57_uf_56_de_inst),
asn(de_load_addr, uf_58_de_i_imm),
asn(de_store_addr, uf_59_de_s_imm),
asn(de_load_wa, uf_60_de_load_addr),
asn(de_store_wa, uf_61_de_store_addr),
asn(de_bytemask, uf_64_uf_63_de_inst),
asn(de_load_byteena, uf_66_uf_65_de_load_addr),
asn(de_store_byteena, uf_68_uf_67_de_store_addr),
asn(de_store, uf_70_uf_69_de_inst),
asn(de_store_local, uf_72_uf_71_de_store_addr),
asn(de_load, uf_74_uf_73_de_inst),
asn(de_rs2_val_shl, uf_76_uf_75_de_store_addr),
asn(bus_csr_readdata_valid, uf_77_const_expr),
asn(ex_ld, uf_79_uf_78_ex_load_addr),
asn(b_addr, uf_81_de_store_wa),
asn(b_din, uf_82_de_rs2_val_shl),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module(bram_tdp, m_mem0, 
[input(m_mem0_a_clk), input(m_mem0_a_wr), input(m_mem0_a_addr), input(m_mem0_a_din), output(m_mem0_a_dout), input(m_mem0_b_clk), input(m_mem0_b_wr), input(m_mem0_b_addr), input(m_mem0_b_din), output(m_mem0_b_dout)], 
[clock, uf_83_const_expr, uf_84_ex_next_pc, uf_85_const_expr, uf_86_if_inst, clock, uf_88_bus_req_write_go, b_addr, uf_89_b_din, uf_90_ex_loaded_data], 
[wire(m_mem0_a_addr), wire(m_mem0_a_clk), wire(m_mem0_a_din), register(m_mem0_a_dout), wire(m_mem0_a_wr), wire(m_mem0_b_addr), wire(m_mem0_b_clk), wire(m_mem0_b_din), register(m_mem0_b_dout), wire(m_mem0_b_wr), register(m_mem0_mem)],
[
],
[
always(event2(posedge,m_mem0_a_clk),
       block([ nb_asn(m_mem0_a_dout, uf_91_m_mem0_a_addr)
             , ite(m_mem0_a_wr,
                   block([ nb_asn(m_mem0_a_dout, m_mem0_a_din)
                         , nb_asn(uf_92_m_mem0_a_addr, m_mem0_a_din)
                         ]),
                   skip)
             ])),
always(event2(posedge,m_mem0_b_clk),
       block([ nb_asn(m_mem0_b_dout, uf_93_m_mem0_b_addr)
             , ite(m_mem0_b_wr,
                   block([ nb_asn(m_mem0_b_dout, m_mem0_b_din)
                         , nb_asn(uf_94_m_mem0_b_addr, m_mem0_b_din)
                         ]),
                   skip)
             ]))
],
[link(uf_91_m_mem0_a_addr, [m_mem0_a_addr, m_mem0_mem]), link(uf_92_m_mem0_a_addr, [m_mem0_a_addr, m_mem0_mem]), link(uf_93_m_mem0_b_addr, [m_mem0_b_addr, m_mem0_mem]), link(uf_94_m_mem0_b_addr, [m_mem0_b_addr, m_mem0_mem])])
%-------------------------------------------------------------------------------
,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module(bram_tdp, m_mem1, 
[input(m_mem1_a_clk), input(m_mem1_a_wr), input(m_mem1_a_addr), input(m_mem1_a_din), output(m_mem1_a_dout), input(m_mem1_b_clk), input(m_mem1_b_wr), input(m_mem1_b_addr), input(m_mem1_b_din), output(m_mem1_b_dout)], 
[clock, uf_95_const_expr, uf_96_ex_next_pc, uf_97_const_expr, uf_98_if_inst, clock, uf_100_bus_req_write_go, b_addr, uf_101_b_din, uf_102_ex_loaded_data], 
[wire(m_mem1_a_addr), wire(m_mem1_a_clk), wire(m_mem1_a_din), register(m_mem1_a_dout), wire(m_mem1_a_wr), wire(m_mem1_b_addr), wire(m_mem1_b_clk), wire(m_mem1_b_din), register(m_mem1_b_dout), wire(m_mem1_b_wr), register(m_mem1_mem)],
[
],
[
always(event2(posedge,m_mem1_a_clk),
       block([ nb_asn(m_mem1_a_dout, uf_103_m_mem1_a_addr)
             , ite(m_mem1_a_wr,
                   block([ nb_asn(m_mem1_a_dout, m_mem1_a_din)
                         , nb_asn(uf_104_m_mem1_a_addr, m_mem1_a_din)
                         ]),
                   skip)
             ])),
always(event2(posedge,m_mem1_b_clk),
       block([ nb_asn(m_mem1_b_dout, uf_105_m_mem1_b_addr)
             , ite(m_mem1_b_wr,
                   block([ nb_asn(m_mem1_b_dout, m_mem1_b_din)
                         , nb_asn(uf_106_m_mem1_b_addr, m_mem1_b_din)
                         ]),
                   skip)
             ]))
],
[link(uf_103_m_mem1_a_addr, [m_mem1_a_addr, m_mem1_mem]), link(uf_104_m_mem1_a_addr, [m_mem1_a_addr, m_mem1_mem]), link(uf_105_m_mem1_b_addr, [m_mem1_b_addr, m_mem1_mem]), link(uf_106_m_mem1_b_addr, [m_mem1_b_addr, m_mem1_mem])])
%-------------------------------------------------------------------------------
,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module(bram_tdp, m_mem2, 
[input(m_mem2_a_clk), input(m_mem2_a_wr), input(m_mem2_a_addr), input(m_mem2_a_din), output(m_mem2_a_dout), input(m_mem2_b_clk), input(m_mem2_b_wr), input(m_mem2_b_addr), input(m_mem2_b_din), output(m_mem2_b_dout)], 
[clock, uf_107_const_expr, uf_108_ex_next_pc, uf_109_const_expr, uf_110_if_inst, clock, uf_112_bus_req_write_go, b_addr, uf_113_b_din, uf_114_ex_loaded_data], 
[wire(m_mem2_a_addr), wire(m_mem2_a_clk), wire(m_mem2_a_din), register(m_mem2_a_dout), wire(m_mem2_a_wr), wire(m_mem2_b_addr), wire(m_mem2_b_clk), wire(m_mem2_b_din), register(m_mem2_b_dout), wire(m_mem2_b_wr), register(m_mem2_mem)],
[
],
[
always(event2(posedge,m_mem2_a_clk),
       block([ nb_asn(m_mem2_a_dout, uf_115_m_mem2_a_addr)
             , ite(m_mem2_a_wr,
                   block([ nb_asn(m_mem2_a_dout, m_mem2_a_din)
                         , nb_asn(uf_116_m_mem2_a_addr, m_mem2_a_din)
                         ]),
                   skip)
             ])),
always(event2(posedge,m_mem2_b_clk),
       block([ nb_asn(m_mem2_b_dout, uf_117_m_mem2_b_addr)
             , ite(m_mem2_b_wr,
                   block([ nb_asn(m_mem2_b_dout, m_mem2_b_din)
                         , nb_asn(uf_118_m_mem2_b_addr, m_mem2_b_din)
                         ]),
                   skip)
             ]))
],
[link(uf_115_m_mem2_a_addr, [m_mem2_a_addr, m_mem2_mem]), link(uf_116_m_mem2_a_addr, [m_mem2_a_addr, m_mem2_mem]), link(uf_117_m_mem2_b_addr, [m_mem2_b_addr, m_mem2_mem]), link(uf_118_m_mem2_b_addr, [m_mem2_b_addr, m_mem2_mem])])
%-------------------------------------------------------------------------------
,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module(bram_tdp, m_mem3, 
[input(m_mem3_a_clk), input(m_mem3_a_wr), input(m_mem3_a_addr), input(m_mem3_a_din), output(m_mem3_a_dout), input(m_mem3_b_clk), input(m_mem3_b_wr), input(m_mem3_b_addr), input(m_mem3_b_din), output(m_mem3_b_dout)], 
[clock, uf_119_const_expr, uf_120_ex_next_pc, uf_121_const_expr, uf_122_if_inst, clock, uf_124_de_store_local, b_addr, uf_125_b_din, uf_126_ex_loaded_data], 
[wire(m_mem3_a_addr), wire(m_mem3_a_clk), wire(m_mem3_a_din), register(m_mem3_a_dout), wire(m_mem3_a_wr), wire(m_mem3_b_addr), wire(m_mem3_b_clk), wire(m_mem3_b_din), register(m_mem3_b_dout), wire(m_mem3_b_wr), register(m_mem3_mem)],
[
],
[
always(event2(posedge,m_mem3_a_clk),
       block([ nb_asn(m_mem3_a_dout, uf_127_m_mem3_a_addr)
             , ite(m_mem3_a_wr,
                   block([ nb_asn(m_mem3_a_dout, m_mem3_a_din)
                         , nb_asn(uf_128_m_mem3_a_addr, m_mem3_a_din)
                         ]),
                   skip)
             ])),
always(event2(posedge,m_mem3_b_clk),
       block([ nb_asn(m_mem3_b_dout, uf_129_m_mem3_b_addr)
             , ite(m_mem3_b_wr,
                   block([ nb_asn(m_mem3_b_dout, m_mem3_b_din)
                         , nb_asn(uf_130_m_mem3_b_addr, m_mem3_b_din)
                         ]),
                   skip)
             ]))
],
[link(uf_127_m_mem3_a_addr, [m_mem3_a_addr, m_mem3_mem]), link(uf_128_m_mem3_a_addr, [m_mem3_a_addr, m_mem3_mem]), link(uf_129_m_mem3_b_addr, [m_mem3_b_addr, m_mem3_mem]), link(uf_130_m_mem3_b_addr, [m_mem3_b_addr, m_mem3_mem])])
%-------------------------------------------------------------------------------
,
asn(bus_req_ready, uf_131_de_store)
],
[
always(event1(star),
       ite(uf_132_interrupt_mask,
           b_asn(interrupt_cause, uf_133_const_expr),
           ite(uf_134_interrupt_mask,
               b_asn(interrupt_cause, uf_135_const_expr),
               ite(uf_136_interrupt_mask,
                   b_asn(interrupt_cause, uf_137_const_expr),
                   ite(uf_138_interrupt_mask,
                       b_asn(interrupt_cause, uf_139_const_expr),
                       ite(uf_140_interrupt_mask,
                           b_asn(interrupt_cause, uf_141_const_expr),
                           ite(uf_142_interrupt_mask,
                               b_asn(interrupt_cause, uf_143_const_expr),
                               ite(uf_144_interrupt_mask,
                                   b_asn(interrupt_cause, uf_145_const_expr),
                                   b_asn(interrupt_cause, uf_146_const_expr))))))))),
always(event2(posedge,clock),
       block([ nb_asn(if_valid_, uf_147_ex_restart)
             , nb_asn(if_pc, ex_next_pc)
             ])),
always(event2(posedge,clock),
       block([ nb_asn(de_valid_, if_valid)
             , nb_asn(de_pc, if_pc)
             , nb_asn(de_inst, if_inst)
             ])),
always(event1(star),
       ite(uf_149_uf_148_de_inst, b_asn(de_csrd, uf_157_uf_156_de_inst),ite(uf_150_uf_148_de_inst, b_asn(de_csrd, uf_158_de_inst),ite(uf_151_uf_148_de_inst, b_asn(de_csrd, uf_159_de_inst),ite(uf_152_uf_148_de_inst, b_asn(de_csrd, uf_160_de_inst),ite(uf_153_uf_148_de_inst, b_asn(de_csrd, uf_161_de_inst),ite(uf_154_uf_148_de_inst, b_asn(de_csrd, uf_162_de_inst),b_asn(de_csrd, uf_163_const_expr)))))))),
always(event1(star),
       ite(uf_165_uf_164_de_inst, b_asn(de_csr_val, csr_fflags),ite(uf_166_uf_164_de_inst, b_asn(de_csr_val, csr_frm),ite(uf_167_uf_164_de_inst, b_asn(de_csr_val, uf_197_csr_fflags),ite(uf_168_uf_164_de_inst, b_asn(de_csr_val, csr_cycle),ite(uf_169_uf_164_de_inst, b_asn(de_csr_val, csr_time),ite(uf_170_uf_164_de_inst, b_asn(de_csr_val, csr_instret),ite(uf_171_uf_164_de_inst, b_asn(de_csr_val, uf_198_csr_cycle),ite(uf_172_uf_164_de_inst, b_asn(de_csr_val, uf_199_csr_time),ite(uf_173_uf_164_de_inst, b_asn(de_csr_val, uf_200_csr_instret),ite(uf_174_uf_164_de_inst, b_asn(de_csr_val, csr_mcpuid),ite(uf_175_uf_164_de_inst, b_asn(de_csr_val, csr_mimpid),ite(uf_176_uf_164_de_inst, b_asn(de_csr_val, csr_mhartid),ite(uf_177_uf_164_de_inst, b_asn(de_csr_val, csr_mstatus),ite(uf_178_uf_164_de_inst, b_asn(de_csr_val, csr_mtvec),ite(uf_179_uf_164_de_inst, b_asn(de_csr_val, csr_mtdeleg),ite(uf_180_uf_164_de_inst, b_asn(de_csr_val, csr_mie),ite(uf_181_uf_164_de_inst, b_asn(de_csr_val, csr_mtimecmp),ite(uf_182_uf_164_de_inst, b_asn(de_csr_val, csr_mscratch),ite(uf_183_uf_164_de_inst, b_asn(de_csr_val, csr_mepc),ite(uf_184_uf_164_de_inst, b_asn(de_csr_val, csr_mcause),ite(uf_185_uf_164_de_inst, b_asn(de_csr_val, csr_mbadaddr),ite(uf_186_uf_164_de_inst, b_asn(de_csr_val, csr_mip),ite(uf_187_uf_164_de_inst, b_asn(de_csr_val, uf_201_const_expr),ite(uf_188_uf_164_de_inst, b_asn(de_csr_val, uf_202_const_expr),ite(uf_189_uf_164_de_inst, b_asn(de_csr_val, uf_203_const_expr),ite(uf_190_uf_164_de_inst, b_asn(de_csr_val, uf_204_const_expr),ite(uf_191_uf_164_de_inst, b_asn(de_csr_val, uf_205_const_expr),ite(uf_192_uf_164_de_inst, b_asn(de_csr_val, uf_206_const_expr),ite(uf_193_uf_164_de_inst, b_asn(de_csr_val, uf_207_const_expr),ite(uf_194_uf_164_de_inst, b_asn(de_csr_val, uf_208_const_expr),ite(uf_195_uf_164_de_inst, b_asn(de_csr_val, csr_mtohost),ite(uf_196_uf_164_de_inst, b_asn(de_csr_val, csr_mfromhost),b_asn(de_csr_val, uf_209_const_expr)))))))))))))))))))))))))))))))))),
always(event1(star),
       ite(uf_210_bus_csr_no, b_asn(bus_csr_readdata, csr_fflags),ite(uf_211_bus_csr_no, b_asn(bus_csr_readdata, csr_frm),ite(uf_212_bus_csr_no, b_asn(bus_csr_readdata, uf_242_csr_fflags),ite(uf_213_bus_csr_no, b_asn(bus_csr_readdata, csr_cycle),ite(uf_214_bus_csr_no, b_asn(bus_csr_readdata, csr_time),ite(uf_215_bus_csr_no, b_asn(bus_csr_readdata, csr_instret),ite(uf_216_bus_csr_no, b_asn(bus_csr_readdata, uf_243_csr_cycle),ite(uf_217_bus_csr_no, b_asn(bus_csr_readdata, uf_244_csr_time),ite(uf_218_bus_csr_no, b_asn(bus_csr_readdata, uf_245_csr_instret),ite(uf_219_bus_csr_no, b_asn(bus_csr_readdata, csr_mcpuid),ite(uf_220_bus_csr_no, b_asn(bus_csr_readdata, csr_mimpid),ite(uf_221_bus_csr_no, b_asn(bus_csr_readdata, csr_mhartid),ite(uf_222_bus_csr_no, b_asn(bus_csr_readdata, csr_mstatus),ite(uf_223_bus_csr_no, b_asn(bus_csr_readdata, csr_mtvec),ite(uf_224_bus_csr_no, b_asn(bus_csr_readdata, csr_mtdeleg),ite(uf_225_bus_csr_no, b_asn(bus_csr_readdata, csr_mie),ite(uf_226_bus_csr_no, b_asn(bus_csr_readdata, csr_mtimecmp),ite(uf_227_bus_csr_no, b_asn(bus_csr_readdata, csr_mscratch),ite(uf_228_bus_csr_no, b_asn(bus_csr_readdata, csr_mepc),ite(uf_229_bus_csr_no, b_asn(bus_csr_readdata, csr_mcause),ite(uf_230_bus_csr_no, b_asn(bus_csr_readdata, csr_mbadaddr),ite(uf_231_bus_csr_no, b_asn(bus_csr_readdata, csr_mip),ite(uf_232_bus_csr_no, b_asn(bus_csr_readdata, uf_246_const_expr),ite(uf_233_bus_csr_no, b_asn(bus_csr_readdata, uf_247_const_expr),ite(uf_234_bus_csr_no, b_asn(bus_csr_readdata, uf_248_const_expr),ite(uf_235_bus_csr_no, b_asn(bus_csr_readdata, uf_249_const_expr),ite(uf_236_bus_csr_no, b_asn(bus_csr_readdata, uf_250_const_expr),ite(uf_237_bus_csr_no, b_asn(bus_csr_readdata, uf_251_const_expr),ite(uf_238_bus_csr_no, b_asn(bus_csr_readdata, uf_252_const_expr),ite(uf_239_bus_csr_no, b_asn(bus_csr_readdata, uf_253_const_expr),ite(uf_240_bus_csr_no, b_asn(bus_csr_readdata, csr_mtohost),ite(uf_241_bus_csr_no, b_asn(bus_csr_readdata, csr_mfromhost),b_asn(bus_csr_readdata, uf_254_const_expr)))))))))))))))))))))))))))))))))),
always(event2(posedge,clock),
       block([ nb_asn(ex_valid_, uf_255_de_illegal_csr_access)
             , nb_asn(ex_inst, de_inst)
             , nb_asn(ex_load_addr, de_load_addr)
             , nb_asn(ex_csrd, de_csrd)
             , nb_asn(ex_load_byteena, de_load_byteena)
             ])),
always(event1(star),
       block([ b_asn(ex_ld_shifted, uf_257_uf_256_ex_load_addr)
             , ite(uf_259_uf_258_ex_inst, b_asn(ex_ld_res, uf_265_uf_264_ex_ld_shifted),ite(uf_260_uf_258_ex_inst, b_asn(ex_ld_res, uf_268_uf_267_ex_ld_shifted),ite(uf_261_uf_258_ex_inst, b_asn(ex_ld_res, uf_269_ex_ld_shifted),ite(uf_262_uf_258_ex_inst, b_asn(ex_ld_res, uf_270_ex_ld_shifted),b_asn(ex_ld_res, ex_ld)))))
             ])),
always(event2(posedge,clock),
       block([ ite(ex_restart,
                   skip,
                   skip)
             , nb_asn(ex_restart, uf_271_const_expr)
             , nb_asn(ex_next_pc, uf_272_ex_next_pc)
             , ite(uf_275_uf_274_de_inst,
                   block([ nb_asn(ex_restart, uf_276_const_expr)
                         , nb_asn(ex_next_pc, uf_277_de_pc)
                         ]),
                   skip)
             , ite(de_illegal_csr_access,
                   block([ nb_asn(ex_restart, uf_278_const_expr)
                         , nb_asn(ex_next_pc, uf_280_uf_279_csr_mstatus)
                         ]),
                   skip)
             , ite(uf_283_uf_282_uf_281_de_inst, ite(de_branch_taken,
                   block([ nb_asn(ex_restart, uf_287_const_expr)
                         , nb_asn(ex_next_pc, uf_288_de_sb_imm)
                         ]),
                   skip),ite(uf_284_uf_282_uf_281_de_inst, block([ nb_asn(ex_restart, uf_289_const_expr)
                     , nb_asn(ex_next_pc, uf_290_de_i_imm)
                     ]),ite(uf_285_uf_282_uf_281_de_inst, block([ nb_asn(ex_restart, uf_291_const_expr)
                     , nb_asn(ex_next_pc, uf_292_de_uj_imm)
                     ]),ite(uf_286_uf_282_uf_281_de_inst, block([ ite(uf_294_uf_293_de_inst, block([ nb_asn(ex_restart, uf_295_const_expr)
                             , ite(uf_297_uf_296_de_inst, nb_asn(ex_next_pc, uf_301_uf_300_csr_mstatus),ite(uf_298_uf_296_de_inst, nb_asn(ex_next_pc, csr_mepc),ite(uf_299_uf_296_de_inst, skip,block([ skip
                                     , skip
                                     ]))))
                             ]),skip)
                     ]),skip))))
             , ite(interrupt,
                   block([ nb_asn(ex_restart, uf_302_const_expr)
                         , nb_asn(ex_next_pc, uf_304_uf_303_csr_mstatus)
                         ]),
                   skip)
             ])),
always(event2(posedge,clock),
       ite(uf_306_uf_305_de_inst, ite(uf_313_uf_312_de_inst, ite(uf_323_uf_322_de_inst,
           nb_asn(ex_res, uf_324_de_rs2_val_imm),
           nb_asn(ex_res, uf_325_de_rs2_val_imm)),ite(uf_314_uf_312_de_inst, nb_asn(ex_res, uf_327_uf_326_de_rs2_val_imm),ite(uf_315_uf_312_de_inst, nb_asn(ex_res, uf_328_de_rs2_val_imm),ite(uf_316_uf_312_de_inst, nb_asn(ex_res, uf_329_de_rs2_val_imm),ite(uf_317_uf_312_de_inst, nb_asn(ex_res, uf_330_de_rs2_val_imm),ite(uf_318_uf_312_de_inst, ite(uf_331_de_inst,
           nb_asn(ex_res, uf_333_uf_332_de_rs2_val_imm),
           nb_asn(ex_res, uf_335_uf_334_de_rs2_val_imm)),ite(uf_319_uf_312_de_inst, nb_asn(ex_res, uf_336_de_rs2_val_imm),ite(uf_320_uf_312_de_inst, nb_asn(ex_res, uf_337_de_rs2_val_imm),skip)))))))),ite(uf_307_uf_305_de_inst, nb_asn(ex_res, uf_339_uf_338_de_inst),ite(uf_308_uf_305_de_inst, nb_asn(ex_res, uf_341_uf_340_de_inst),ite(uf_309_uf_305_de_inst, nb_asn(ex_res, uf_342_de_pc),ite(uf_310_uf_305_de_inst, nb_asn(ex_res, uf_343_de_pc),ite(uf_311_uf_305_de_inst, ite(uf_345_uf_344_de_inst, block([ nb_asn(ex_res, de_csr_val)
             , nb_asn(ex_csr_res, uf_351_de_rs1_val)
             ]),ite(uf_346_uf_344_de_inst, block([ nb_asn(ex_res, de_csr_val)
             , nb_asn(ex_csr_res, uf_352_de_rs1_val)
             ]),ite(uf_347_uf_344_de_inst, block([ nb_asn(ex_res, de_csr_val)
             , nb_asn(ex_csr_res, de_rs1_val)
             ]),ite(uf_348_uf_344_de_inst, block([ nb_asn(ex_res, de_csr_val)
             , nb_asn(ex_csr_res, uf_354_uf_353_de_inst)
             ]),ite(uf_349_uf_344_de_inst, block([ nb_asn(ex_res, de_csr_val)
             , nb_asn(ex_csr_res, uf_356_uf_355_de_inst)
             ]),ite(uf_350_uf_344_de_inst, block([ nb_asn(ex_res, de_csr_val)
             , nb_asn(ex_csr_res, uf_357_de_inst)
             ]),skip)))))),skip))))))),
always(event1(star),
       b_asn(ex_wbv, uf_359_ex_res)),
always(event1(star),
       b_asn(ex_wben, uf_363_uf_362_ex_inst)),
always(event2(posedge,clock),
       block([ ite(ex_wben,
                   nb_asn(uf_365_uf_364_ex_inst, ex_wbv),
                   skip)
             , nb_asn(de_rs1_val_r, uf_367_uf_366_if_inst)
             , nb_asn(de_rs2_val_r, uf_369_uf_368_if_inst)
             ])),
always(event2(posedge,clock),
       block([ nb_asn(wb_valid, ex_valid)
             , nb_asn(wb_inst, ex_inst)
             , nb_asn(wb_wben, ex_wben)
             , nb_asn(wb_wbv, ex_wbv)
             , nb_asn(wb_csrd, ex_csrd)
             ])),
always(event2(posedge,clock),
       block([ ite(uf_370_csr_mtimecmp,
                   block([ skip
                         , ite(uf_372_uf_371_csr_mie,
                               skip,
                               skip)
                         , nb_asn(uf_373_csr_mip, uf_374_const_expr)
                         ]),
                   skip)
             , nb_asn(csr_cycle, uf_375_csr_cycle)
             , nb_asn(csr_time, uf_376_csr_time)
             , nb_asn(csr_mtime, uf_377_csr_mtime)
             , nb_asn(csr_instret, uf_378_ex_valid)
             , ite(de_illegal_csr_access,
                   skip,
                   skip)
             , ite(uf_379_interrupt,
                   block([ ite(uf_381_uf_380_ex_inst,
                               block([ skip
                                     , ite(uf_382_ex_csrd, nb_asn(csr_fflags, ex_csr_res),ite(uf_383_ex_csrd, nb_asn(csr_frm, ex_csr_res),ite(uf_384_ex_csrd, nb_asn(uf_399_csr_fflags, ex_csr_res),ite(uf_385_ex_csrd, nb_asn(csr_mstatus, uf_400_ex_csr_res),ite(uf_386_ex_csrd, nb_asn(csr_mie, ex_csr_res),ite(uf_387_ex_csrd, nb_asn(csr_mtimecmp, ex_csr_res),ite(uf_388_ex_csrd, nb_asn(csr_mscratch, ex_csr_res),ite(uf_389_ex_csrd, nb_asn(csr_mepc, ex_csr_res),ite(uf_390_ex_csrd, nb_asn(uf_401_csr_mip, uf_402_ex_csr_res),ite(uf_391_ex_csrd, nb_asn(csr_mfromhost, ex_csr_res),ite(uf_392_ex_csrd, block([ nb_asn(csr_mtohost, ex_csr_res)
                                             , skip
                                             , skip
                                             ]),ite(uf_393_ex_csrd, nb_asn(csr_cycle, ex_csr_res),ite(uf_394_ex_csrd, nb_asn(csr_time, ex_csr_res),ite(uf_395_ex_csrd, nb_asn(csr_instret, ex_csr_res),ite(uf_396_ex_csrd, nb_asn(uf_403_csr_cycle, ex_csr_res),ite(uf_397_ex_csrd, nb_asn(uf_404_csr_time, ex_csr_res),ite(uf_398_ex_csrd, nb_asn(uf_405_csr_instret, ex_csr_res),skip)))))))))))))))))
                                     ]),
                               skip)
                         , ite(uf_409_uf_408_ex_inst,
                               nb_asn(uf_410_csr_mstatus, uf_411_csr_mstatus),
                               skip)
                         ]),
                   skip)
             , ite(uf_415_uf_414_de_i_imm,
                   block([ nb_asn(csr_mepc, de_pc)
                         , nb_asn(uf_416_csr_mstatus, uf_417_csr_mstatus)
                         , nb_asn(uf_418_csr_mstatus, uf_419_const_expr)
                         , nb_asn(uf_420_csr_mstatus, uf_421_const_expr)
                         , nb_asn(csr_mcause, uf_424_uf_422_de_i_imm)
                         ]),
                   skip)
             ])),
always(event1(star),
       block([ b_asn(writedata, de_rs2_val_shl)
             , b_asn(byteena, uf_425_de_store)
             , b_asn(writeenable, de_store)
             , b_asn(readenable, uf_428_uf_427_de_load_addr)
             , b_asn(address, uf_431_uf_430_de_load_addr)
             ])),
always(event2(posedge,clock),
       block([ nb_asn(ex_loaded_data_is_bus_res, bus_req_read_go)
             , nb_asn(bus_res_valid, ex_loaded_data_is_bus_res)
             , nb_asn(bus_res_data, ex_loaded_data)
             ]))
],
[link(uf_0_bus_req_read, [bus_req_read, bus_req_ready]), link(uf_1_bus_req_write, [bus_req_write, bus_req_ready]), link(uf_2_bus_req_write_go, [bus_req_write_go, bus_req_read_go]), link(uf_3_csr_mstatus, [csr_mstatus]), link(uf_4_csr_mie, [csr_mie, uf_3_csr_mstatus, csr_mip]), link(uf_5_csr_mip, [csr_mip, csr_mie]), link(uf_6_ex_restart, [ex_restart, if_valid_]), link(uf_7_ex_restart, [ex_restart, de_valid_]), link(uf_8_de_inst, [de_inst]), link(uf_9_de_inst, [de_inst]), link(uf_10_csr_mstatus, [csr_mstatus]), link(uf_11_de_inst, [de_inst]), link(uf_12_de_inst, [de_inst]), link(uf_13_de_inst, [de_inst]), link(uf_14_de_inst, [de_inst]), link(uf_15_uf_14_de_inst, [uf_14_de_inst, uf_8_de_inst, de_valid, uf_11_de_inst, uf_9_de_inst, uf_13_de_inst, uf_10_csr_mstatus, uf_12_de_inst]), link(uf_16_de_inst, [de_inst]), link(uf_17_ex_inst, [ex_inst]), link(uf_18_uf_16_de_inst, [uf_16_de_inst, ex_wben, uf_17_ex_inst]), link(uf_19_de_inst, [de_inst]), link(uf_20_ex_inst, [ex_inst]), link(uf_21_uf_19_de_inst, [uf_19_de_inst, ex_wben, uf_20_ex_inst]), link(uf_22_de_inst, [de_inst]), link(uf_23_wb_inst, [wb_inst]), link(uf_24_wb_wben, [wb_wben, uf_22_de_inst, uf_23_wb_inst]), link(uf_25_de_inst, [de_inst]), link(uf_26_wb_inst, [wb_inst]), link(uf_27_wb_wben, [wb_wben, uf_25_de_inst, uf_26_wb_inst]), link(uf_28_de_rs1_val_r, [de_rs1_val_r, wb_wbv, de_rs1_forward_wb, de_rs1_forward_ex, ex_wbv]), link(uf_29_de_rs2_val_r, [de_rs2_val_r, wb_wbv, de_rs2_forward_wb, de_rs2_forward_ex, ex_wbv]), link(uf_30_de_inst, [de_inst]), link(uf_31_de_rs1_val, [de_rs1_val, uf_30_de_inst]), link(uf_32_de_inst, [de_inst]), link(uf_33_de_rs2_val, [de_rs2_val, uf_32_de_inst]), link(uf_34_de_rs2_val, [de_rs2_val, de_rs1_val]), link(uf_35_de_rs2_val_cmp, [de_rs2_val_cmp, de_rs1_val_cmp]), link(uf_36_de_inst, [de_inst]), link(uf_37_de_inst, [de_inst]), link(uf_38_de_cmp_eq, [de_cmp_eq, uf_37_de_inst, uf_36_de_inst, de_cmp_lt]), link(uf_39_de_inst, [de_inst]), link(uf_40_de_sign, [de_sign]), link(uf_41_de_sign, [de_sign]), link(uf_42_de_inst, [de_inst]), link(uf_43_de_inst, [de_inst]), link(uf_44_uf_43_de_inst, [uf_43_de_inst, de_sign20, uf_42_de_inst]), link(uf_45_de_inst, [de_inst]), link(uf_46_de_inst, [de_inst]), link(uf_47_uf_46_de_inst, [uf_46_de_inst, de_sign20, uf_45_de_inst]), link(uf_48_de_inst, [de_inst]), link(uf_49_de_inst, [de_inst]), link(uf_50_de_inst, [de_inst]), link(uf_51_uf_50_de_inst, [uf_50_de_inst, uf_49_de_inst, de_sign20, uf_48_de_inst]), link(uf_52_de_inst, [de_inst]), link(uf_53_de_inst, [de_inst]), link(uf_54_de_inst, [de_inst]), link(uf_55_uf_54_de_inst, [uf_54_de_inst, de_sign12, uf_53_de_inst, uf_52_de_inst]), link(uf_56_de_inst, [de_inst]), link(uf_57_uf_56_de_inst, [uf_56_de_inst, de_rs2_val, de_i_imm]), link(uf_58_de_i_imm, [de_i_imm, de_rs1_val]), link(uf_59_de_s_imm, [de_s_imm, de_rs1_val]), link(uf_60_de_load_addr, [de_load_addr]), link(uf_61_de_store_addr, [de_store_addr]), link(uf_62_de_inst, [de_inst]), link(uf_63_de_inst, [de_inst]), link(uf_64_uf_63_de_inst, [uf_63_de_inst, uf_62_de_inst]), link(uf_65_de_load_addr, [de_load_addr]), link(uf_66_uf_65_de_load_addr, [uf_65_de_load_addr, de_bytemask]), link(uf_67_de_store_addr, [de_store_addr]), link(uf_68_uf_67_de_store_addr, [uf_67_de_store_addr, de_bytemask]), link(uf_69_de_inst, [de_inst]), link(uf_70_uf_69_de_inst, [uf_69_de_inst, de_valid]), link(uf_71_de_store_addr, [de_store_addr]), link(uf_72_uf_71_de_store_addr, [uf_71_de_store_addr, de_store]), link(uf_73_de_inst, [de_inst]), link(uf_74_uf_73_de_inst, [uf_73_de_inst, de_valid]), link(uf_75_de_store_addr, [de_store_addr]), link(uf_76_uf_75_de_store_addr, [uf_75_de_store_addr, de_rs2_val]), link(uf_77_const_expr, []), link(uf_78_ex_load_addr, [ex_load_addr]), link(uf_79_uf_78_ex_load_addr, [uf_78_ex_load_addr, ex_loaded_data, readdata]), link(uf_80_bus_req_address, [bus_req_address]), link(uf_81_de_store_wa, [de_store_wa, de_store_local, de_load_wa, bus_req_rw_go, uf_80_bus_req_address]), link(uf_82_de_rs2_val_shl, [de_rs2_val_shl, bus_req_write_go, bus_req_data]), link(uf_83_const_expr, []), link(uf_84_ex_next_pc, [ex_next_pc]), link(uf_85_const_expr, []), link(uf_86_if_inst, [if_inst]), link(uf_87_de_store_byteena, [de_store_byteena]), link(uf_88_bus_req_write_go, [bus_req_write_go, de_store_local, uf_87_de_store_byteena]), link(uf_89_b_din, [b_din]), link(uf_90_ex_loaded_data, [ex_loaded_data]), link(uf_95_const_expr, []), link(uf_96_ex_next_pc, [ex_next_pc]), link(uf_97_const_expr, []), link(uf_98_if_inst, [if_inst]), link(uf_99_de_store_byteena, [de_store_byteena]), link(uf_100_bus_req_write_go, [bus_req_write_go, de_store_local, uf_99_de_store_byteena]), link(uf_101_b_din, [b_din]), link(uf_102_ex_loaded_data, [ex_loaded_data]), link(uf_107_const_expr, []), link(uf_108_ex_next_pc, [ex_next_pc]), link(uf_109_const_expr, []), link(uf_110_if_inst, [if_inst]), link(uf_111_de_store_byteena, [de_store_byteena]), link(uf_112_bus_req_write_go, [bus_req_write_go, de_store_local, uf_111_de_store_byteena]), link(uf_113_b_din, [b_din]), link(uf_114_ex_loaded_data, [ex_loaded_data]), link(uf_119_const_expr, []), link(uf_120_ex_next_pc, [ex_next_pc]), link(uf_121_const_expr, []), link(uf_122_if_inst, [if_inst]), link(uf_123_de_store_byteena, [de_store_byteena]), link(uf_124_de_store_local, [de_store_local, bus_req_write_go, uf_123_de_store_byteena]), link(uf_125_b_din, [b_din]), link(uf_126_ex_loaded_data, [ex_loaded_data]), link(uf_131_de_store, [de_store, de_load]), link(uf_132_interrupt_mask, [interrupt_mask]), link(uf_133_const_expr, []), link(uf_134_interrupt_mask, [interrupt_mask]), link(uf_135_const_expr, []), link(uf_136_interrupt_mask, [interrupt_mask]), link(uf_137_const_expr, []), link(uf_138_interrupt_mask, [interrupt_mask]), link(uf_139_const_expr, []), link(uf_140_interrupt_mask, [interrupt_mask]), link(uf_141_const_expr, []), link(uf_142_interrupt_mask, [interrupt_mask]), link(uf_143_const_expr, []), link(uf_144_interrupt_mask, [interrupt_mask]), link(uf_145_const_expr, []), link(uf_146_const_expr, []), link(uf_147_ex_restart, [ex_restart, if_valid]), link(uf_148_de_inst, [de_inst]), link(uf_149_uf_148_de_inst, [uf_148_de_inst]), link(uf_150_uf_148_de_inst, [uf_148_de_inst]), link(uf_151_uf_148_de_inst, [uf_148_de_inst]), link(uf_152_uf_148_de_inst, [uf_148_de_inst]), link(uf_153_uf_148_de_inst, [uf_148_de_inst]), link(uf_154_uf_148_de_inst, [uf_148_de_inst]), link(uf_155_de_inst, [de_inst]), link(uf_156_de_inst, [de_inst]), link(uf_157_uf_156_de_inst, [uf_156_de_inst, uf_155_de_inst]), link(uf_158_de_inst, [de_inst]), link(uf_159_de_inst, [de_inst]), link(uf_160_de_inst, [de_inst]), link(uf_161_de_inst, [de_inst]), link(uf_162_de_inst, [de_inst]), link(uf_163_const_expr, []), link(uf_164_de_inst, [de_inst]), link(uf_165_uf_164_de_inst, [uf_164_de_inst]), link(uf_166_uf_164_de_inst, [uf_164_de_inst]), link(uf_167_uf_164_de_inst, [uf_164_de_inst]), link(uf_168_uf_164_de_inst, [uf_164_de_inst]), link(uf_169_uf_164_de_inst, [uf_164_de_inst]), link(uf_170_uf_164_de_inst, [uf_164_de_inst]), link(uf_171_uf_164_de_inst, [uf_164_de_inst]), link(uf_172_uf_164_de_inst, [uf_164_de_inst]), link(uf_173_uf_164_de_inst, [uf_164_de_inst]), link(uf_174_uf_164_de_inst, [uf_164_de_inst]), link(uf_175_uf_164_de_inst, [uf_164_de_inst]), link(uf_176_uf_164_de_inst, [uf_164_de_inst]), link(uf_177_uf_164_de_inst, [uf_164_de_inst]), link(uf_178_uf_164_de_inst, [uf_164_de_inst]), link(uf_179_uf_164_de_inst, [uf_164_de_inst]), link(uf_180_uf_164_de_inst, [uf_164_de_inst]), link(uf_181_uf_164_de_inst, [uf_164_de_inst]), link(uf_182_uf_164_de_inst, [uf_164_de_inst]), link(uf_183_uf_164_de_inst, [uf_164_de_inst]), link(uf_184_uf_164_de_inst, [uf_164_de_inst]), link(uf_185_uf_164_de_inst, [uf_164_de_inst]), link(uf_186_uf_164_de_inst, [uf_164_de_inst]), link(uf_187_uf_164_de_inst, [uf_164_de_inst]), link(uf_188_uf_164_de_inst, [uf_164_de_inst]), link(uf_189_uf_164_de_inst, [uf_164_de_inst]), link(uf_190_uf_164_de_inst, [uf_164_de_inst]), link(uf_191_uf_164_de_inst, [uf_164_de_inst]), link(uf_192_uf_164_de_inst, [uf_164_de_inst]), link(uf_193_uf_164_de_inst, [uf_164_de_inst]), link(uf_194_uf_164_de_inst, [uf_164_de_inst]), link(uf_195_uf_164_de_inst, [uf_164_de_inst]), link(uf_196_uf_164_de_inst, [uf_164_de_inst]), link(uf_197_csr_fflags, [csr_fflags, csr_frm]), link(uf_198_csr_cycle, [csr_cycle]), link(uf_199_csr_time, [csr_time]), link(uf_200_csr_instret, [csr_instret]), link(uf_201_const_expr, []), link(uf_202_const_expr, []), link(uf_203_const_expr, []), link(uf_204_const_expr, []), link(uf_205_const_expr, []), link(uf_206_const_expr, []), link(uf_207_const_expr, []), link(uf_208_const_expr, []), link(uf_209_const_expr, []), link(uf_210_bus_csr_no, [bus_csr_no]), link(uf_211_bus_csr_no, [bus_csr_no]), link(uf_212_bus_csr_no, [bus_csr_no]), link(uf_213_bus_csr_no, [bus_csr_no]), link(uf_214_bus_csr_no, [bus_csr_no]), link(uf_215_bus_csr_no, [bus_csr_no]), link(uf_216_bus_csr_no, [bus_csr_no]), link(uf_217_bus_csr_no, [bus_csr_no]), link(uf_218_bus_csr_no, [bus_csr_no]), link(uf_219_bus_csr_no, [bus_csr_no]), link(uf_220_bus_csr_no, [bus_csr_no]), link(uf_221_bus_csr_no, [bus_csr_no]), link(uf_222_bus_csr_no, [bus_csr_no]), link(uf_223_bus_csr_no, [bus_csr_no]), link(uf_224_bus_csr_no, [bus_csr_no]), link(uf_225_bus_csr_no, [bus_csr_no]), link(uf_226_bus_csr_no, [bus_csr_no]), link(uf_227_bus_csr_no, [bus_csr_no]), link(uf_228_bus_csr_no, [bus_csr_no]), link(uf_229_bus_csr_no, [bus_csr_no]), link(uf_230_bus_csr_no, [bus_csr_no]), link(uf_231_bus_csr_no, [bus_csr_no]), link(uf_232_bus_csr_no, [bus_csr_no]), link(uf_233_bus_csr_no, [bus_csr_no]), link(uf_234_bus_csr_no, [bus_csr_no]), link(uf_235_bus_csr_no, [bus_csr_no]), link(uf_236_bus_csr_no, [bus_csr_no]), link(uf_237_bus_csr_no, [bus_csr_no]), link(uf_238_bus_csr_no, [bus_csr_no]), link(uf_239_bus_csr_no, [bus_csr_no]), link(uf_240_bus_csr_no, [bus_csr_no]), link(uf_241_bus_csr_no, [bus_csr_no]), link(uf_242_csr_fflags, [csr_fflags, csr_frm]), link(uf_243_csr_cycle, [csr_cycle]), link(uf_244_csr_time, [csr_time]), link(uf_245_csr_instret, [csr_instret]), link(uf_246_const_expr, []), link(uf_247_const_expr, []), link(uf_248_const_expr, []), link(uf_249_const_expr, []), link(uf_250_const_expr, []), link(uf_251_const_expr, []), link(uf_252_const_expr, []), link(uf_253_const_expr, []), link(uf_254_const_expr, []), link(uf_255_de_illegal_csr_access, [de_illegal_csr_access, de_valid]), link(uf_256_ex_load_addr, [ex_load_addr]), link(uf_257_uf_256_ex_load_addr, [uf_256_ex_load_addr, ex_ld]), link(uf_258_ex_inst, [ex_inst]), link(uf_259_uf_258_ex_inst, [uf_258_ex_inst]), link(uf_260_uf_258_ex_inst, [uf_258_ex_inst]), link(uf_261_uf_258_ex_inst, [uf_258_ex_inst]), link(uf_262_uf_258_ex_inst, [uf_258_ex_inst]), link(uf_263_ex_ld_shifted, [ex_ld_shifted]), link(uf_264_ex_ld_shifted, [ex_ld_shifted]), link(uf_265_uf_264_ex_ld_shifted, [uf_264_ex_ld_shifted, uf_263_ex_ld_shifted]), link(uf_266_ex_ld_shifted, [ex_ld_shifted]), link(uf_267_ex_ld_shifted, [ex_ld_shifted]), link(uf_268_uf_267_ex_ld_shifted, [uf_267_ex_ld_shifted, uf_266_ex_ld_shifted]), link(uf_269_ex_ld_shifted, [ex_ld_shifted]), link(uf_270_ex_ld_shifted, [ex_ld_shifted]), link(uf_271_const_expr, []), link(uf_272_ex_next_pc, [ex_next_pc]), link(uf_273_de_inst, [de_inst]), link(uf_274_de_inst, [de_inst]), link(uf_275_uf_274_de_inst, [uf_274_de_inst, de_csrd, de_valid, uf_273_de_inst]), link(uf_276_const_expr, []), link(uf_277_de_pc, [de_pc]), link(uf_278_const_expr, []), link(uf_279_csr_mstatus, [csr_mstatus]), link(uf_280_uf_279_csr_mstatus, [uf_279_csr_mstatus, csr_mtvec]), link(uf_281_de_inst, [de_inst]), link(uf_282_uf_281_de_inst, [uf_281_de_inst, de_valid, interrupt]), link(uf_283_uf_282_uf_281_de_inst, [uf_282_uf_281_de_inst]), link(uf_284_uf_282_uf_281_de_inst, [uf_282_uf_281_de_inst]), link(uf_285_uf_282_uf_281_de_inst, [uf_282_uf_281_de_inst]), link(uf_286_uf_282_uf_281_de_inst, [uf_282_uf_281_de_inst]), link(uf_287_const_expr, []), link(uf_288_de_sb_imm, [de_sb_imm, de_pc]), link(uf_289_const_expr, []), link(uf_290_de_i_imm, [de_i_imm, de_rs1_val]), link(uf_291_const_expr, []), link(uf_292_de_uj_imm, [de_uj_imm, de_pc]), link(uf_293_de_inst, [de_inst]), link(uf_294_uf_293_de_inst, [uf_293_de_inst]), link(uf_295_const_expr, []), link(uf_296_de_inst, [de_inst]), link(uf_297_uf_296_de_inst, [uf_296_de_inst]), link(uf_298_uf_296_de_inst, [uf_296_de_inst]), link(uf_299_uf_296_de_inst, [uf_296_de_inst]), link(uf_300_csr_mstatus, [csr_mstatus]), link(uf_301_uf_300_csr_mstatus, [uf_300_csr_mstatus, csr_mtvec]), link(uf_302_const_expr, []), link(uf_303_csr_mstatus, [csr_mstatus]), link(uf_304_uf_303_csr_mstatus, [uf_303_csr_mstatus, csr_mtvec]), link(uf_305_de_inst, [de_inst]), link(uf_306_uf_305_de_inst, [uf_305_de_inst]), link(uf_307_uf_305_de_inst, [uf_305_de_inst]), link(uf_308_uf_305_de_inst, [uf_305_de_inst]), link(uf_309_uf_305_de_inst, [uf_305_de_inst]), link(uf_310_uf_305_de_inst, [uf_305_de_inst]), link(uf_311_uf_305_de_inst, [uf_305_de_inst]), link(uf_312_de_inst, [de_inst]), link(uf_313_uf_312_de_inst, [uf_312_de_inst]), link(uf_314_uf_312_de_inst, [uf_312_de_inst]), link(uf_315_uf_312_de_inst, [uf_312_de_inst]), link(uf_316_uf_312_de_inst, [uf_312_de_inst]), link(uf_317_uf_312_de_inst, [uf_312_de_inst]), link(uf_318_uf_312_de_inst, [uf_312_de_inst]), link(uf_319_uf_312_de_inst, [uf_312_de_inst]), link(uf_320_uf_312_de_inst, [uf_312_de_inst]), link(uf_321_de_inst, [de_inst]), link(uf_322_de_inst, [de_inst]), link(uf_323_uf_322_de_inst, [uf_322_de_inst, uf_321_de_inst]), link(uf_324_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_325_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_326_de_rs2_val_imm, [de_rs2_val_imm]), link(uf_327_uf_326_de_rs2_val_imm, [uf_326_de_rs2_val_imm, de_rs1_val]), link(uf_328_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_329_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_330_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_331_de_inst, [de_inst]), link(uf_332_de_rs2_val_imm, [de_rs2_val_imm]), link(uf_333_uf_332_de_rs2_val_imm, [uf_332_de_rs2_val_imm, de_rs1_val]), link(uf_334_de_rs2_val_imm, [de_rs2_val_imm]), link(uf_335_uf_334_de_rs2_val_imm, [uf_334_de_rs2_val_imm, de_rs1_val]), link(uf_336_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_337_de_rs2_val_imm, [de_rs2_val_imm, de_rs1_val]), link(uf_338_de_inst, [de_inst]), link(uf_339_uf_338_de_inst, [uf_338_de_inst]), link(uf_340_de_inst, [de_inst]), link(uf_341_uf_340_de_inst, [uf_340_de_inst, de_pc]), link(uf_342_de_pc, [de_pc]), link(uf_343_de_pc, [de_pc]), link(uf_344_de_inst, [de_inst]), link(uf_345_uf_344_de_inst, [uf_344_de_inst]), link(uf_346_uf_344_de_inst, [uf_344_de_inst]), link(uf_347_uf_344_de_inst, [uf_344_de_inst]), link(uf_348_uf_344_de_inst, [uf_344_de_inst]), link(uf_349_uf_344_de_inst, [uf_344_de_inst]), link(uf_350_uf_344_de_inst, [uf_344_de_inst]), link(uf_351_de_rs1_val, [de_rs1_val, de_csr_val]), link(uf_352_de_rs1_val, [de_rs1_val, de_csr_val]), link(uf_353_de_inst, [de_inst]), link(uf_354_uf_353_de_inst, [uf_353_de_inst, de_csr_val]), link(uf_355_de_inst, [de_inst]), link(uf_356_uf_355_de_inst, [uf_355_de_inst, de_csr_val]), link(uf_357_de_inst, [de_inst]), link(uf_358_ex_inst, [ex_inst]), link(uf_359_ex_res, [ex_res, uf_358_ex_inst, ex_ld_res]), link(uf_360_ex_inst, [ex_inst]), link(uf_361_ex_inst, [ex_inst]), link(uf_362_ex_inst, [ex_inst]), link(uf_363_uf_362_ex_inst, [uf_362_ex_inst, uf_361_ex_inst, ex_valid, uf_360_ex_inst]), link(uf_364_ex_inst, [ex_inst]), link(uf_365_uf_364_ex_inst, [uf_364_ex_inst, regs]), link(uf_366_if_inst, [if_inst]), link(uf_367_uf_366_if_inst, [uf_366_if_inst, regs]), link(uf_368_if_inst, [if_inst]), link(uf_369_uf_368_if_inst, [uf_368_if_inst, regs]), link(uf_370_csr_mtimecmp, [csr_mtimecmp, csr_mtime]), link(uf_371_csr_mie, [csr_mie]), link(uf_372_uf_371_csr_mie, [uf_371_csr_mie]), link(uf_373_csr_mip, [csr_mip]), link(uf_374_const_expr, []), link(uf_375_csr_cycle, [csr_cycle]), link(uf_376_csr_time, [csr_time]), link(uf_377_csr_mtime, [csr_mtime]), link(uf_378_ex_valid, [ex_valid, csr_instret]), link(uf_379_interrupt, [interrupt, ex_valid]), link(uf_380_ex_inst, [ex_inst]), link(uf_381_uf_380_ex_inst, [uf_380_ex_inst, ex_csrd]), link(uf_382_ex_csrd, [ex_csrd]), link(uf_383_ex_csrd, [ex_csrd]), link(uf_384_ex_csrd, [ex_csrd]), link(uf_385_ex_csrd, [ex_csrd]), link(uf_386_ex_csrd, [ex_csrd]), link(uf_387_ex_csrd, [ex_csrd]), link(uf_388_ex_csrd, [ex_csrd]), link(uf_389_ex_csrd, [ex_csrd]), link(uf_390_ex_csrd, [ex_csrd]), link(uf_391_ex_csrd, [ex_csrd]), link(uf_392_ex_csrd, [ex_csrd]), link(uf_393_ex_csrd, [ex_csrd]), link(uf_394_ex_csrd, [ex_csrd]), link(uf_395_ex_csrd, [ex_csrd]), link(uf_396_ex_csrd, [ex_csrd]), link(uf_397_ex_csrd, [ex_csrd]), link(uf_398_ex_csrd, [ex_csrd]), link(uf_399_csr_fflags, [csr_fflags, csr_frm]), link(uf_400_ex_csr_res, [ex_csr_res]), link(uf_401_csr_mip, [csr_mip]), link(uf_402_ex_csr_res, [ex_csr_res]), link(uf_403_csr_cycle, [csr_cycle]), link(uf_404_csr_time, [csr_time]), link(uf_405_csr_instret, [csr_instret]), link(uf_406_ex_inst, [ex_inst]), link(uf_407_ex_inst, [ex_inst]), link(uf_408_ex_inst, [ex_inst]), link(uf_409_uf_408_ex_inst, [uf_408_ex_inst, uf_406_ex_inst, uf_407_ex_inst]), link(uf_410_csr_mstatus, [csr_mstatus]), link(uf_411_csr_mstatus, [csr_mstatus]), link(uf_412_de_inst, [de_inst]), link(uf_413_de_inst, [de_inst]), link(uf_414_de_i_imm, [de_i_imm]), link(uf_415_uf_414_de_i_imm, [uf_414_de_i_imm, uf_413_de_inst, de_illegal_csr_access, uf_412_de_inst, de_valid, interrupt]), link(uf_416_csr_mstatus, [csr_mstatus]), link(uf_417_csr_mstatus, [csr_mstatus]), link(uf_418_csr_mstatus, [csr_mstatus]), link(uf_419_const_expr, []), link(uf_420_csr_mstatus, [csr_mstatus]), link(uf_421_const_expr, []), link(uf_422_de_i_imm, [de_i_imm]), link(uf_423_csr_mstatus, [csr_mstatus]), link(uf_424_uf_422_de_i_imm, [uf_422_de_i_imm, de_illegal_csr_access, uf_423_csr_mstatus, interrupt, interrupt_cause]), link(uf_425_de_store, [de_store, de_load_byteena, de_store_byteena]), link(uf_426_de_inst, [de_inst]), link(uf_427_de_load_addr, [de_load_addr]), link(uf_428_uf_427_de_load_addr, [uf_427_de_load_addr, de_valid, uf_426_de_inst]), link(uf_429_de_store_addr, [de_store_addr]), link(uf_430_de_load_addr, [de_load_addr]), link(uf_431_uf_430_de_load_addr, [uf_430_de_load_addr, de_store, uf_429_de_store_addr])]).


%-------------------------------------------------------------------------------

% skipping variable initializations
taint_source(if_inst).

sanitize_glob(if_inst).

taint_sink(de_inst).

taint_sink(ex_inst).

sanitize(csr_fflags).

sanitize(csr_frm).

sanitize(csr_cycle).

sanitize(csr_time).

sanitize(csr_instret).

sanitize(csr_mcpuid).

sanitize(csr_mimpid).

sanitize(csr_mhartid).

sanitize(csr_mstatus).

sanitize(csr_mtvec).

sanitize(csr_mtdeleg).

sanitize(csr_mie).

sanitize(csr_mtimecmp).

sanitize(csr_mtime).

sanitize(csr_mscratch).

sanitize(csr_mepc).

sanitize(csr_mcause).

sanitize(csr_mbadaddr).

sanitize(csr_mip).

sanitize(csr_mtohost).

sanitize(csr_mfromhost).

sanitize(interrupt_cause).

sanitize(ex_restart).

sanitize(ex_next_pc).

sanitize(ex_valid_).

sanitize(ex_inst).

sanitize(ex_wben).

sanitize(ex_wbv).

sanitize(wb_valid).

sanitize(wb_inst).

sanitize(wb_wben).

sanitize(wb_wbv).

sanitize(if_valid_).

sanitize(if_pc).

sanitize(de_valid_).

sanitize(de_pc).

sanitize(de_inst).

sanitize(de_csr_val).

sanitize(de_rs1_val_r).

sanitize(de_rs2_val_r).

sanitize(de_csrd).

sanitize(ex_load_addr).

sanitize(ex_csrd).

sanitize(ex_csr_res).

sanitize(ex_load_byteena).

sanitize(wb_csrd).

sanitize(ex_ld_shifted, ex_ld_res).

sanitize(ex_res).

sanitize(ex_loaded_data_is_bus_res).

sanitize(ex_pc, ex_sb_imm, ex_i_imm, ex_s_imm, ex_uj_imm).

