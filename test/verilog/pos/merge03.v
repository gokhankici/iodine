module test(clk, sel, opa, opb, out);
   input wire clk, sel;
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
   input wire [31:0] opa, opb;

   // @annot{taint_sink(out)}
   output reg [31:0] out;

   // @annot{qualifier2([opa_r, opb_r])}
   // @annot{qualifier2([r1, r2])}

   reg opa_r, opb_r;
   
   always @(posedge clk)
     opa_r <= opa;
   
   always @(posedge clk)
     opb_r <= opb;

   reg r1,r2;
   
   always @(*)
     r1 = opa_r + opb_r;
   
   always @(*)
     r2 = opa_r - opb_r;

   always @(posedge clk)
      if (sel)
        out <= r1;
      else
        out <= r2;

endmodule
