`timescale 10ns / 100ps

module proj4_testbench;
  reg clk, reset;

  mips_pipeline p1(clk, reset);

  always
    #5 clk = ~clk;

  initial begin
    clk = 1'b0;
    reset = 1'b0; #10;
    reset = 1'b1; #10;
    reset = 1'b0;
  end

endmodule
