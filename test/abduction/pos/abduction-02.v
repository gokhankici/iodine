module test(clk);
   // @annot{taint_source(x)}
   // @annot{taint_sink(z)}

   input clk, slow;
   reg   x;                     
   reg   y;
   reg   z;                     

   always @(posedge clk)
     if (slow == 1)
        x <= x;
     else 
        y <= x;

   always @(posedge clk)
     z <= y;
   
endmodule
