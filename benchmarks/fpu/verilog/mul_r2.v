`timescale 1ns / 100ps

module mul_r2(clk, opa, opb, prod);
input		clk;
input	[23:0]	opa, opb;
output	[47:0]	prod;

// @annot{taint_source(opa)}
// @annot{taint_source(opb)}

// @annot{taint_sink(rewrite_prod)}

// --------------------------------------------------------------------------------
// REWRITE
// --------------------------------------------------------------------------------
reg [47:0] rewrite_prod;
always @(*) begin
  rewrite_prod = prod;
end
// --------------------------------------------------------------------------------

reg	[47:0]	prod1, prod;

always @(posedge clk)
	prod1 <= #1 opa * opb;

always @(posedge clk)
	prod <= #1 prod1;

endmodule
