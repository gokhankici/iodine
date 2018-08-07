module test(clk, in1, in2, out);
   input wire clk;
   input wire [1:0] in1;
   input wire [1:0] in2;
   output reg [1:0] out = 2;
   wire             fast;
   reg [1:0]        temp1 = 3;

   // @annot{taint_source(in1)}
   // @annot{taint_source(in2)}
   // @annot{taint_sink(out)}

   // @annot{qualifierImp(temp1, [in1, in2])}
   
   assign fast = (in1 | in2) == 0;
   
   always @ (posedge clk) begin
      temp1 <= in1 + in2;
   end
   
   always @ (posedge clk) begin
      if (fast)
        out <= 0;
      else
        out <= temp1;
   end
endmodule

