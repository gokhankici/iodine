module test(clk);
   input clk, slow;
   reg   slow;                  // @annot{sanitize(v_slow)}
   reg   x;                     // @annot{taint_source(v_x)}
   reg   y;
   reg   z;                     // @annot{taint_sink(v_z)}

   always @(posedge clk) begin
      if (slow == 1)
        x = 1;
      else 
        y = 1;

      z = x + y;
   end
   
endmodule
