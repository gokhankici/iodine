module test(clk);
   input clk, slow;
   reg   slow;                  // @annot{sanitize(v_slow)}
   reg   x;                     // @annot{taint_source(v_x)}
   reg   y;                     // @annot{taint_sink(v_y)}

   always @(posedge clk)
     if (slow == 1)
        x <= x;
     else 
        y <= x;
   
endmodule