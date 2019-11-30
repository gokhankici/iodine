module test(clk, x, y);
   input wire clk;
   input wire x;
   output reg [3:0] y;

   always @(posedge clk) begin
      y[3] <= x;
      if (~x)
        y[2:0] <= 2'b00;
   end
endmodule
