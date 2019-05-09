module test(clk);
   input clk;

   // @annot{taint_source(ex_inst)}
   // @annot{taint_sink(wb_wbv)}

   reg   ex_inst; 
   reg   ex_ld_res;
   reg   ex_res;
   reg   wb_wbv, ex_wbv;
   reg   not_eq, cond, not_eq0, not_eq1;

   always @(posedge clk) begin
      not_eq0 <= ex_inst + 1;
      not_eq1 <= ex_inst + 2;
      
      if (cond)
        not_eq <= not_eq0;
      else
        not_eq <= not_eq1;
   end

   reg foo1,foo2;
   
   always @(posedge clk)
      if (cond)
        ex_res <= ex_inst + 1;
      else
        ex_res <= ex_inst + 2;

   always @(posedge clk)
     ex_ld_res <= not_eq;

   always @(posedge clk)
     ex_wbv <= ex_inst[0] == 0 ? ex_ld_res : ex_res;

   always @(posedge clk) begin
      wb_wbv <= ex_wbv;
   end

endmodule
