module modular(clk);
   input clk, slow;
   reg   slow;                  // @annot{sanitize(v_slow)}
   reg   x;                     // @annot{taint_source(v_x)}
   reg   y;
   reg   z;                     // @annot{taint_sink(v_y)}
   reg   a;
   reg   b;

   always @(posedge clk)
     if (slow == 1)
       y <= x;
     else 
       z <= x;
   
   always @(posedge clk)
     if (slow)
       z <= y;

   other OTHER(clk);

endmodule

module other(clk);
   input clk;
   reg   a;
   reg   b;

   always @(posedge clk)
     a <= b;
endmodule
   
