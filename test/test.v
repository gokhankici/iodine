module test(clk);
   input clk;

   // @annot{taint_source(a_)}
   // @annot{taint_sink(r)}
   // @annot{taint_eq(b)}

   reg a_;

   wire a,b,c;
   reg  r;
   
   assign a = a_;
   assign c = a & b;

   always @(posedge clk) begin
      r <= c;
   end
   

endmodule
