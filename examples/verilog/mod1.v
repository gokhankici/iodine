module mod1(clk);
   input clk;
   reg   a; // source
   reg   b;
   reg   c; // sink

   always @(posedge clk)
     b <= a + 2;
   
   always @(posedge clk)
     c <= b * 4;
endmodule
