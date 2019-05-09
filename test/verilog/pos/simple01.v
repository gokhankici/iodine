module test(clk);
   input clk;
   reg   x;                     // @annot{taint_source(x)}
   reg   y;                    // @annot{taint_sink(y)}

   always @(posedge clk) begin
      y <= x;
   end
   
endmodule
