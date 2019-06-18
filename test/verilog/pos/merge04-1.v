module test(clk, in1, in2, in3, out);
   input wire clk, in1, in2, in3;
   output reg out;

   reg in1_r, in2_r, in3_r;

   always @(posedge clk) begin
     in1_r <= in1;
     in2_r <= in2;
     in3_r <= in3;
     if (in3_r)
       out <= in1_r;
     else
       out <= in1_r + in2_r;
   end

endmodule
