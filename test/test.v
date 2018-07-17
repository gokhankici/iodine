module test(clk);
   input clk;

   // @annot{taint_source(in)}
   reg in;

   // @annot{taint_sink(out)}
   reg out;

   // always @(*) begin
   //         out = in;
   // end

   wire a,b,c;
   reg  r;
   
   assign c = a & b;

   always @(posedge clk) begin
      r <= c;
   end
   

endmodule
