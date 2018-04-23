module test(clk);
   input clk;

   // {H}
   // @annot{taint_source(in)}
   // @annot{sanitize(in)}
   reg in;

   // {H}
   // @annot{taint_sink(out)}
   reg out;

    // if you enable the following annotation, it passes
    // otherwise, it fails
    // @annot-not{sanitize(slow)}
   reg slow; 

   // {H}
   reg tmp;

   always @(posedge(clk)) begin
	   if (slow) begin
		   tmp <= in;
		   out <= tmp; 
	   end 
       else 
		   out <= in;
   end

endmodule
