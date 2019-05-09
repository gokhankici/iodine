module alu_stub(ctl, a, b, result, zero);
   // inputs are the source:
   // @annot{taint_source(ctl)}
   input [2:0]  ctl;
   input [31:0] a, b;

   // outputs are the sink:
   // @annot{taint_sink(result)} @annot{taint_sink(zero)}
   output [31:0] result;
   output        zero;

   reg [31:0]    result;
   reg           zero;

   always @(*) begin
      result = ctl + a + b;
   end
endmodule

