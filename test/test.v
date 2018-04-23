module test(clk);
   input clk;

   // @annot{taint_source(in)}
   reg in;

   // @annot{taint_sink(out)}
   reg out;

   always @(*) begin
	   out = in + 1;
	   out = in + 2;
   end

endmodule
