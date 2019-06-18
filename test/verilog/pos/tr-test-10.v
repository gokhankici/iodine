module test(clk, i1, i2, o);
   input wire clk, i1, i2;
   output reg o;
   reg cond;

   always @(posedge clk)
     if (cond)
       o <= i1;
     else
       o <= i2;

endmodule
