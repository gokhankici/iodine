module test(clk, slow, x);
   // @annot{taint_source(x)}
   // @annot{taint_sink(y)}

   input clk, slow, x;
   reg   y;

   always @(posedge clk)
     if (slow == 1) begin
        y <= y;
     end else begin
       y <= x;
     end

endmodule
