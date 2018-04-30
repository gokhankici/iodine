module test(clk, i, o);
   input  wire clk;
   input  wire i; // @annot{taint_source(i)}
   output reg  o; // @annot{taint_sink(o)}

   reg x;

   wire a1 = x + 1;
   wire b1 = x + 2;

   always @(posedge clk)
	   x <= x + 1;

   always @(posedge clk) begin
	   if (a1 > 0)
		   o <= b1;
	   else
		   o <= 0;
   end

endmodule
