module test(clk);
   input clk;
   reg   x;                     // @annot{taint_source(x)}
   reg   y;                     // @annot{taint_sink(y)}

   reg a,b;

   always @(posedge clk) begin
	   if (a)
		   b <= 1;
	   else
		   b <= 1;
   end

   always @(posedge clk) begin
      y <= x + b;
   end

endmodule
