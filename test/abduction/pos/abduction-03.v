module test(clk, in1, in2, out);
   // @annot{taint_source(in1)}
   // @annot{taint_source(in2)}
   // @annot{taint_sink(out)}

   // @annot{sanitize(cond)}
   // @annot{sanitize_glob(in2)}

   input clk, in1, in2;
   output out;
   
   reg    out, cond, in1_1;

   always @(posedge clk)
     in1_1 <= in1;

   always @(posedge clk)
     cond <= in2;

   always @(posedge clk)
     if (cond) begin
        out <= 0;
     end else begin
        out <= in1_1;
     end

endmodule
