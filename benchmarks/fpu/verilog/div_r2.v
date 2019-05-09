`timescale 1ns / 100ps

module div_r2(clk, opa, opb, quo, rem);
input		clk;
input	[49:0]	opa;
input	[23:0]	opb;
output	[49:0]	quo, rem;

// @annot{taint_source(opa)}
// @annot{taint_source(opb)}

// @annot{taint_sink(quo)}
// @annot{taint_sink(rem)}

reg	[49:0]	quo, rem, quo1, remainder;

always @(posedge clk)
	quo1 <= #1 opa / opb;

always @(posedge clk)
	quo <= #1 quo1;

always @(posedge clk)
	remainder <= #1 opa % opb;

always @(posedge clk)
	rem <= #1 remainder;

endmodule


