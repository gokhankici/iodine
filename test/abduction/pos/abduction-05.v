module test(clk, flag1, flag2, flag3, flag4, in, out);
   
   // @annot{taint_source(flag1)}
   // @annot{taint_source(flag2)}
   // @annot{taint_source(flag3)}
   // @annot{taint_source(flag4)}
   // @annot{taint_source(in)}

   // @annot{taint_sink(out)}

   // @annot{sanitize_glob(cond)}

   input clk, flag1, flag2, flag3, flag4, in;
   output out;

   reg out;

   reg cond, tmp;

   always @(posedge clk)
     cond <= flag1 + flag2 + flag3 + flag4;

   always @(posedge clk)
     tmp <= in;
   
   always @(posedge clk)
     if (cond) begin
        out <= 0;
     end else begin
        out <= tmp;
     end

endmodule
