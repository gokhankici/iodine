`include "_parameter.v"

module r_mem (
	input	[`ADDR_WIDTH32 - 1 : 0]  address,
	input   clock,
	input 	[`DATA_WIDTH32 - 1 : 0] qi,
	output	[`DATA_WIDTH32 - 1 : 0] q);

	assign q = qi + address;

endmodule


module n_mem (
	input	[`ADDR_WIDTH32 - 1 : 0]  address,
	input	clock,
	input	[`DATA_WIDTH32 - 1 : 0]  data,
	input 	[`DATA_WIDTH32 - 1 : 0] qi,
	output	[`DATA_WIDTH32 - 1 : 0]  q);

	assign q = data + qi + address;

endmodule

module t_mem (
	input	[`ADDR_WIDTH32 - 1 : 0]  address,
	input	clock,
	input 	[`DATA_WIDTH32 - 1 : 0] qi,
	output	[`DATA_WIDTH32 - 1 : 0]  q);

	assign q = qi + address;

endmodule

module nprime0_mem (
	input	[1 : 0]  address,
	input	clock,
	input	[`DATA_WIDTH32 - 1 : 0]  data,
	input 	[`DATA_WIDTH32 - 1 : 0] qi,
	output	[`DATA_WIDTH32 - 1 : 0]  q);

	assign q = data + qi + address;

endmodule