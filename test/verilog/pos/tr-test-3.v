module test(clk);
   input clk, slow;
   reg   slow;                  // @annot{sanitize(slow)}
   reg   x;                     // @annot{taint_source(x)}
   reg   y;
   reg   z;                     // @annot{taint_sink(z)}

   always @(posedge clk)
     if (slow == 1)
        x <= x;
     else 
        y <= x;

   always @(posedge clk)
     z <= y;
   
endmodule
