module test(clk);

// =============================================================================
// ANNOTATIONS
// =============================================================================
// taint sources & sink
// @annot{taint_source(in)}
// @annot{taint_source(wallclock)}
// @annot{taint_sink(out)}
// =============================================================================
// assumptions
// @annot{sanitize_glob(in)}
// @annot{sanitize(in, out, wallclock, state, inH)}
// =============================================================================
// we can prove this module if we enable the following annotation:
// @annot-not{sanitize(secret)}
// =============================================================================

input  wire       clk; // L
reg[1:0]  in;  // L
reg[63:0] out; // L

reg[63:0] wallclock; // L

// state = 0 ==> read input
// state = 1 ==> compute step 1
// state = 2 ==> compute step 2

// ZL(0)=L, ZL(n>0)=H
reg[1:0]  state = 0; // ZL state

reg[31:0] secret; // H
reg[1:0]  inH;    // H

always @(posedge clk) begin
	wallclock <= wallclock + 1;

	case (state)
		0: begin 
             inH <= in; 
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
