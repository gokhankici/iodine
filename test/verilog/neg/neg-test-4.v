module test(clk, x);
   input clk;
   input x;

   // @annot{sanitize(ex_res)} 
   // @annot{taint_source(ex_inst)}
   reg   ex_inst; 

   reg   ex_ld_res;

   // @annot{sanitize(ex_res)}
   reg   ex_res;

   // @annot{sanitize(wb_wbv, ex_wbv)}
   // @annot{taint_sink(wb_wbv)}
   reg   wb_wbv, ex_wbv;

   always @(*)
     ex_ld_res[1:0] = x;

   always @(*)
     ex_ld_res[2:0] = x;

   always @(*) ex_wbv = ex_inst[0] == 0 ? ex_ld_res : ex_res;

   always @(posedge clk) begin
      wb_wbv <= ex_wbv;
   end

endmodule
