module test(clk, i1, i2, o);
   // @annot{taint_source(i1)}
   // @annot{taint_source(i2)}
   // @annot{taint_sink(o)}
   
   input wire clk, i1, i2;
   output reg o;
   reg cond;

   // @annot{qualifierImp(i1,[i2])}
   // @annot{qualifierImp(i2,[i1])}

   always @(posedge clk)
     if (cond)
       o <= i1;
     else
       o <= i2;

endmodule
