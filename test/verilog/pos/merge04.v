module test(clk, in1, in2, in3, out);
   input wire clk, in1, in2, in3;
   output reg out;
   
   reg in1_r, in2_r, in3_r;
   reg in1_r2, in2_r2, in3_r2;
   reg out_tmp;

   always @(posedge clk)
     in1_r <= in1;

   always @(posedge clk)
     in2_r <= in2;

   always @(posedge clk)
     in3_r <= in3;

   always @(posedge clk)
     in1_r2 <= in1_r;

   always @(posedge clk)
     in2_r2 <= in2_r;

   always @(posedge clk)
     in3_r2 <= in3_r;

   always @(*)
     if (in3_r2)
       out_tmp = in1_r2;
     else
       out_tmp = in1_r2 + in2_r2;

   always @(posedge clk)
     out <= out_tmp;

endmodule
