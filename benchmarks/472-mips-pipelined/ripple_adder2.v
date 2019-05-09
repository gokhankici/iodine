module fulladder(a, b, cin, sum, cout);
  input a, b, cin;
  output sum, cout;
  assign #2 sum = a ^ b ^ cin;
  assign #2 cout = a & b | a & cin | b & cin;
endmodule

module fulladder_LA(a, b, cin, sum, p, g);
  input a, b, cin;
  output sum, p, g;
  assign #2 sum = a ^ b ^ cin;
  assign #1 g = a & b;
  assign #1 p = a ^ b;
endmodule

module lookahead(p3, p2, p1, p0, g3, g2, g1, g0, c0, c1, c2, c3, c4, P, G);
  input p3, p2, p1, p0, g3, g2, g1, g0, c0;
  output c1, c2, c3, c4, P, G;
  
  // Hook up the carries
  assign #2 c1 = g0 | p0 & c0;
  assign #2 c2 = g1 | p1 & c1;
  assign #2 c3 = g2 | p2 & c2;
  assign #2 c4 = g3 | p3 & c3;
  
  // Logic for "Super Propagate" and "Super Generate"
  assign #3 P = p3 & p2 & p1 & p0;
  assign #4 G = g3 | p3 & g2 | p3 & p2 & g1 | p3 & p2 & p1 & g0;
endmodule

module CLA(a, b, cin, sum, cout, P, G);
  input [3:0] a, b;
  input cin;
  output [3:0] sum;
  output cout, P, G;
  
  wire [3:0] c, p, g;
  assign c[0] = cin;
  
  fulladder_LA f0(a[0], b[0], c[0], sum[0], p[0], g[0]);
  fulladder_LA f1(a[1], b[1], c[1], sum[1], p[1], g[1]);
  fulladder_LA f2(a[2], b[2], c[2], sum[2], p[2], g[2]);
  fulladder_LA f3(a[3], b[3], c[3], sum[3], p[3], g[3]);
  
  lookahead LA(p[3], p[2], p[1], p[0], g[3], g[2], g[1], g[0], c[0], c[1], c[2], c[3], cout, P, G);
endmodule

module add16_LA(a, b, cin, sum, cout, P, G);
  input [15:0] a, b;
  input cin;
  output [15:0] sum;
  output cout, P, G;

  wire [3:0] c, p, g;
  assign c[0] = cin;

  // The carry out of each CLA remains unconnected, as we will use the p and g instead
  CLA cla0(a[3:0],   b[3:0],   c[0], sum[3:0],   , p[0], g[0]);
  CLA cla1(a[7:4],   b[7:4],   c[1], sum[7:4],   , p[1], g[1]);
  CLA cla2(a[11:8],  b[11:8],  c[2], sum[11:8],  , p[2], g[2]);
  CLA cla3(a[15:12], b[15:12], c[3], sum[15:12], , p[3], g[3]);

  lookahead LA(p[3], p[2], p[1], p[0], g[3], g[2], g[1], g[0], c[0], c[1], c[2], c[3], cout, P, G);
endmodule

module ripple_adder(a, b, sum, cout);
  input [15:0] a, b;
  output [15:0] sum;
  output cout;
  
  wire [15:0] c;
  assign c[0] = 0;
  
  fulladder f0(a[0], b[0], c[0], sum[0], c[1]);
  fulladder f1(a[1], b[1], c[1], sum[1], c[2]);
  fulladder f2(a[2], b[2], c[2], sum[2], c[3]);
  fulladder f3(a[3], b[3], c[3], sum[3], c[4]);
  fulladder f4(a[4], b[4], c[4], sum[4], c[5]);
  fulladder f5(a[5], b[5], c[5], sum[5], c[6]);
  fulladder f6(a[6], b[6], c[6], sum[6], c[7]);
  fulladder f7(a[7], b[7], c[7], sum[7], c[8]);
  fulladder f8(a[8], b[8], c[8], sum[8], c[9]);
  fulladder f9(a[9], b[9], c[9], sum[9], c[10]);
  fulladder f10(a[10], b[10], c[10], sum[10], c[11]);
  fulladder f11(a[11], b[11], c[11], sum[11], c[12]);
  fulladder f12(a[12], b[12], c[12], sum[12], c[13]);
  fulladder f13(a[13], b[13], c[13], sum[13], c[14]);
  fulladder f14(a[14], b[14], c[14], sum[14], c[15]);
  fulladder f15(a[15], b[15], c[15], sum[15], cout);
endmodule

module proj1_testbench;
  wire [15:0] sum;
  wire cout;
  
  reg [15:0] A, B;
  reg clk;
  
  // instantiate a ripple_adder (DUT = Device Under Test)
  ripple_adder DUT(A, B, sum, cout);
  
  // Generate a clock that changes every 5 time units (a period of 10)
  always
    #32 clk = ~clk;

  // Initialize signals
  initial begin
    // Initialize clk to 0
    clk = 1'b0;
    
    // Set A and B to 16 bit zeros
    A = 16'hFFF8;
    B = 16'hFFFF;
  end

  // add one to the value of A at each positive clock edge
  always @(posedge clk)
    A = A + 1;
endmodule

module proj1_testbench_CLA;
  wire[15:0] sum;
  
  reg [15:0] A, B;
  reg cin;
  reg clk;
  
  wire cout, P, G;
  
  add16_LA DUT(A, B, cin, sum, cout, P, G);
  
  always
   #32 clk = ~clk;
  
  initial begin
    // Initialize clk to 0
    clk = 1'b0;
    
    // Set A and B to 16 bit zeros
    A = 16'hFFF8;
    B = 16'hFFFF;

    cin = 1'b0;
  
  end

  // add one to the value of A at each positive clock edge
  always @(posedge clk)
    A = A + 1;
endmodule
