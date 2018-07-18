module test(clk, a, b, o);
   input clk, a, b;
   output o;

   // @annot{taint_source(a)}
   // @annot{taint_sink(r)}

   wire c;
   reg  r;
   
   assign c = a & b;

   always @(posedge clk) begin
      r <= c;
   end
   

endmodule
