%-- definitions
register(if_inst).
register(ex_inst).
register(wb_res).
register(regs).

%-- links
asn(if_inst, ex_inst).
link(ex_inst, regs, wb_res).

%-- tag source and sink
source(if_inst).
sink(wb_res).
