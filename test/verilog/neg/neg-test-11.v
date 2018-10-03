module test(clk);
   input clk;
   reg   x;                     // @annot{taint_source(x)}
   reg   y;                     // @annot{taint_source(y)}
   reg   z;                     // @annot{taint_sink(z)}

   // @annot{sanitize_glob(x)}

   // @annot{sanitize(z)}
   // @annot{assert_eq(z)}

   always @(posedge clk) begin
      z <= x + y;
   end

endmodule
