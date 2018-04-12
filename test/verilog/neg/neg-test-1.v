module test(clk);
   input clk;
   reg   x; // @annot{taint_source(x)}
   reg   y; // @annot{taint_sink(y)}

   reg Stall;

   always @(posedge clk) begin
	   if (Stall) 
		   y <= y;
	   else
		   y <= x;
   end

endmodule
