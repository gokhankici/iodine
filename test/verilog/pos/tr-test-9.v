module test(clk, a, b, ar, o);
   input clk, a, b;
   output o;
   input wire ar;

   // @annot{taint_source(a)}
   // @annot{taint_sink(r)}

   wire c;
   reg  r;
   
   assign c = a & b;

   always @(posedge clk) begin
      r <= c;
   end
   

endmodule
