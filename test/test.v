module test(clk);
   input clk;

   // @annot{taint_source(a)}
   // @annot{taint_sink(r)}

   // @annot{taint_eq(a)}
   // @annot{taint_eq(b)}

   wire a,b,c;
   reg  r;
   
   assign c = a & b;

   always @(posedge clk) begin
      r <= c;
   end
   

endmodule
