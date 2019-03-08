module test(clk);
   input clk, slow;
   reg   slow;                  
   reg   x;                     // @annot{taint_source(x)}
   reg   y;                     // @annot{taint_sink(y)}

   always @(posedge clk)
     if (slow == 1)
        x <= x;
     else 
        y <= x;
   
endmodule
