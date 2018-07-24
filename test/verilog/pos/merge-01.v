module test(clk, opa, opb, out);
   input wire clk;
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
   input wire [31:0] opa, opb;

   // @annot{taint_sink(out)}
   output reg [31:0] out;

   reg x, y;

   assign w1a = opa[31];
   assign w1b = opb[31];

   assign w2a = opa[30];
   assign w2b = opb[30];
   
   always @(*) begin
      x = w1a + w1b;
   end

   always @(posedge clk) begin
      if(w2a | w2b)
        y <= 0;
      else
        y <= x;
   end

   always @(posedge clk)
     out <= y;

endmodule
