module test();

reg {L} public, x;
reg {H} secret;
reg {LH x} y;

always @(*)
	if (x == 1) y <= secret;
	else        public <= y;
end

endmodule
