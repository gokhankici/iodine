module test(clk, in1, in2, in3, out);
   // @annot{taint_source(in1)}
   // @annot{taint_source(in2)}
   // @annot{taint_source(in3)}
   
   // @annot{taint_sink(out)}

   // @annot{sanitize(tmp1)}
   // @annot{sanitize(cond)}
   // @annot{sanitize(tmp2)}

   // @annot{sanitize_glob(in2)}
   // @annot{sanitize_glob(in3)}
   
   input clk, in1, in2, in3;
   output out;
   
   reg    out;
   reg    in1_1, in1_2, tmp1, tmp2, cond;
   
   always @(posedge clk) begin
      in1_1 <= in1;
      in1_2 <= in1_1;
      tmp1  <= in2 + 1;
      cond  <= in3 & 1'hF;
   end
   
   always @(posedge clk)
     if (cond)
       tmp2 <= 0;
     else
       tmp2 <= tmp1;
   
   always @(posedge clk)
     if (tmp2)
       out <= 0;
     else
       out <= in1_2;
   
endmodule
