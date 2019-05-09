%-- definitions
register(if_inst).
register(dec_inst).
register(ex_inst).
register(dec_rsa).
register(dec_rsb).
register(ex_res).
register(regs).
register(wb_res).


%-- if stage
asn(if_inst, dec_inst).
link(ex_inst, regs, dec_rsa).
link(ex_inst, regs, dec_rsb).

%-- dec stage
asn(dec_inst, ex_inst).
link(dec_rsa, dec_rsb, ex_res).

%-- ex stage
link(ex_inst, ex_res, wb_res).

%-- wb stage

%-- tag source and sink
source(if_inst).
sink(wb_res).
