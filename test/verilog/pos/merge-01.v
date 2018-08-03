module test(clk, opa, opb, fast, out);
   input wire clk;
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
   input wire [31:0] opa, opb;
   input wire fast;

   // @annot{taint_sink(out)}
   output reg [31:0] out;

   reg r;
   reg opa_r, opb_r;

   always @(posedge clk) begin
      opa_r <= opa;
      opb_r <= opa;
   end 

   assign wa   = opa_r[31];
   assign wb   = opb_r[31];
   assign cond = (wa | wb) == 0;
   
   always @(posedge clk) begin
      r <= opa_r + opb_r;
   end

   always @(posedge clk) begin
      if (cond)
        out <= 0;
      else
        out <= r;
   end

endmodule
