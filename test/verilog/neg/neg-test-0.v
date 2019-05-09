// module example(clk, in1, in2, out);
module example(clk, out);
   input wire clk;
   // input wire [3:0] in1, in2;
   reg [3:0] in1, in2;
   output reg [3:0] out;
   wire fast;
   reg [3:0] temp;

   // @annot{taint_source(in1)}
   // @annot{taint_source(in2)}
   // @annot{taint_sink(out)}

   always @(posedge clk) begin
	   temp <= in1 + in2;
   end

   assign fast = (in1 | in2) == 0;

   always @(posedge clk) begin
	   if (fast) 
		   out <= 0;
	   else
		   out <= temp;
   end

endmodule
