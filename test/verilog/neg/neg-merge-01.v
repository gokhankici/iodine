module test(clk, opa, opb, fast, out);
   input wire clk;
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
   input wire [31:0] opa, opb;
   input wire fast;

   // @annot{taint_sink(out)}
   output reg [31:0] out;

   reg x, y;

   assign wa = opa[31];
   assign wb = opb[31];
   
   always @(*) begin
      x = wa + wb;
   end

   always @(posedge clk)
     if (fast) begin
       y   <= x;
       out <= y;
     end 
     else
       out <= x;
       

endmodule
