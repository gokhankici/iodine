module test(clk);
   input clk;
   reg   x;
   reg   y; 

   // @annot{taint_source(x)}
   // @annot{taint_sink(y)}

   // @annot{sanitize_glob(x)}
   // @annot{sanitize(Stall)}

   reg   Stall;

   always @(posedge clk) begin
      if (Stall) 
        y <= y;
      else
        y <= x;
   end

   wire Stall_wire;

   // @annot{sanitize_mod(reg_file, val)}
   reg_file REG_FILE(clk, 0, Stall_wire);

   always @(*) begin
     Stall <= Stall_wire;
   end

endmodule

module reg_file(clk, regNo, val);
   input wire clk;
   input wire regNo;
   output reg val;

   // 10 x 32-bit registers
   reg [31:0] reg_array [9:0];

   always @(posedge clk ) begin
      val <= reg_array[regNo];
   end
endmodule
