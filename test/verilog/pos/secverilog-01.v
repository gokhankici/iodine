module test(clk);
   input clk;

   // {L}
   // @annot{taint_source(in)}
   // @annot{sanitize(in)}
   reg in;

   // {L}
   // @annot{taint_sink(out)}
   // @annot{sanitize(out)}
   reg out;

   // {H}
   // @annot{sanitize(sec)}
   reg sec;

   always @(posedge(clk)) begin
	   out <= in+sec;
   end

endmodule
