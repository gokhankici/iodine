module test(clk);
   input clk;
   reg   x;                     // @annot{taint_source(x)}
   reg   y;
   reg   z;                     // @annot{taint_sink(z)}

   always @(posedge clk) begin
      y <= x;
   end

   always @(posedge clk) begin
      z <= y;
   end
   
endmodule
