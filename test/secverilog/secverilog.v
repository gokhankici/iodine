module bug(clk, in, out);

input  wire      {L} clk;
input  wire[1:0] {L} in;
output reg[63:0] {L} out;

reg[63:0] {L} wallclock;

// state = 0 ==> read input
// state = 1 ==> compute step 1
// state = 2 ==> compute step 2
reg[1:0] {ZL state} state = 0; // ZL(0)=L, ZL(n>0)=H

reg[31:0] {H} secret;
reg[1:0]  {H} inH;

always @(posedge clk) begin
	wallclock <= wallclock + 1;

	case (state)
		0: begin 
			 inH   <= in; 
		     if (in[0] == 0) begin
				 out   <= 0;
				 state <= 1; 
			 end else begin
				 out   <= wallclock;
				 state <= 0; 
			 end
		   end 
		1: begin 
		     if (secret == 0 || inH == 0) begin
				 state  <= 0; 
				 secret <= 0;
			 end else begin
				 state  <= 2; 
			 end
	       end
		2: begin 
		     state  <= 0;
			 secret <= secret * inH;
		   end
	endcase
end


endmodule
