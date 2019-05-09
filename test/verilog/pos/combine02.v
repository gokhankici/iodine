module test(clk, y, z);
   input  wire clk;
   input  wire y; // @annot{taint_source(y)}
   output reg  z; // @annot{taint_sink(z)}

   reg x;

   wire a;
   assign a = x;

   always @(posedge clk)
	   x <= y;

   always @(posedge clk)
	   z <= a;

endmodule
