module test(clk, slow);
   input clk, slow;
   reg   slow;                 // @annot{sanitize(slow)}
   reg   x;                     // @annot{taint_source(x)}
   // reg   y;
   // reg   z;
   reg   y;                     // @annot{taint_sink(y)}

   always @(posedge clk)
     if (slow)
        y <= y;
     else 
        y <= x;

   // always @(*)
   //   z = y;

   // always @(posedge clk)
   //   t <= z;
   
endmodule
