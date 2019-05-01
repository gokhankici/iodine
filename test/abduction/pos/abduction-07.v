module test(clk);
   // @annot{taint_source(a)}
   // @annot{taint_sink(d)}

   input clk;
   
   reg   a,b,c,d;

   always @(posedge clk) begin
      b <= a + d;
      c <= b;
      d <= c;
   end
     
endmodule
