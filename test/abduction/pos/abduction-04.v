module test(clk, in, a, out);
   // @annot{taint_source(in)}
   // @annot{taint_source(a)}

   // @annot{taint_sink(out)}

   // @annot{sanitize(cond1)}
   // @annot{sanitize(cond2)}
   // @annot{sanitize_glob(a)}

   input clk, in, a;
   output out;

   reg out;

   reg in_1, cond1, cond2;

   always @(posedge clk) begin
      in_1 <= in;
      cond1 <= (a & 1'hF) == 1'h0;
      cond2 <= (a & 1'hF) == 1'hA;
   end
   
   always @(posedge clk)
     if (cond1 && cond2) begin
        out <= 0;
     end else begin
        out <= in_1;
     end

endmodule
