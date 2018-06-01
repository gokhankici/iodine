module test(clk, in_eq, in_neq);
   input clk, in_eq, in_neq;

   // @annot{taint_source(x)}
   // @annot{taint_source(y)}
   // @annot{taint_sink(out)}

   // @annot{sanitize_glob(in_eq)}

   reg   x; // @annot{sanitize(x)}
   reg   y; // @annot{sanitize(y)}
   reg out; // @annot{taint_source(x)}

   reg cond1; // @annot{sanitize(cond1)}
   reg cond2; // @annot{sanitize(cond2)}

   always @(posedge clk) begin
    cond1 <= in_neq;
    cond2 <= in_eq;

    x <= in_eq;
    y <= in_eq;
   end

   always @(posedge clk) begin
     out <= 0;

	   if (cond1) 
		   out <= x;

	   if (cond2) 
		   out <= y;

   end

endmodule
