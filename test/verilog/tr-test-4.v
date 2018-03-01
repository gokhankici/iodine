module test(clk);
   input clk, slow;
   reg   slow;                  // @annot{sanitize(slow)}
   reg   x;                     // @annot{taint_source(x)}
   reg   y;                     // @annot{taint_sink(y)}

   always @(posedge clk)
     y = x;
   
endmodule
