`include "mux3.v"

module minimal(clk,reset);
    input clk, reset;

    wire [31:0] ID_extend, ID_rd1, ID_rd2;
    wire [4:0] ID_rs, ID_rt, ID_rd;
    reg [31:0] file_array [31:1];
    reg [1:0] ForwardA;
    reg  [31:0] EX_rd1;
    wire ID_Branch, ID_MemRead;
    wire [31:0]  EX_offset, EX_btgt, EX_alub, EX_ALUOut;

	// id 2
	always @(posedge clk)		    // ID/EX Pipeline Register
	begin
		if (reset)
		begin
			EX_Branch   <= 0;
			EX_MemRead  <= 0;
			EX_rd1      <= 0;
		end
		else begin
			EX_Branch   <= ID_Branch;
			EX_MemRead  <= ID_MemRead;
			EX_rd1      <= ID_rd1;
		end
	end

	// id 31
	mux3 #(32)  FMUXA(ForwardA, EX_rd1, WB_wd, MEM_ALUOut, MuxA_out);

	// id 4
	always @(posedge clk)		    // EX/MEM Pipeline Register
	begin
		if (reset)
		begin
			MEM_Branch   <= 0;
			MEM_MemRead  <= 0;
			MEM_Zero     <= 0;

			MEM_btgt     <= 0;
			MEM_ALUOut   <= 0;
			MEM_RegRd    <= 0;
		end
		else begin
			MEM_Branch   <= EX_Branch;
			MEM_MemRead  <= EX_MemRead;
			MEM_Zero     <= EX_Zero;

			MEM_btgt     <= EX_btgt;
			MEM_ALUOut   <= EX_ALUOut;
			MEM_RegRd    <= EX_RegRd;
		end
	end

	// id 38
	and  		MEM_BR_AND(MEM_PCSrc, MEM_Branch, MEM_Zero);

	// id 15
	mux2 #(32)	IF_PCMUX(MEM_PCSrc, IF_pc_jump, MEM_btgt, IF_pc_next);

	// id 11
	reg32		IF_PC(clk, reset, IF_pc_next, IF_pc);

	// for 18 & 16 : address is the formal parameter, IF_pc is the actual parameter

	// id 0
	always @(posedge clk) begin
		if (reset) begin
			ID_instr <= 0;
			ID_pc4   <= 0;
		end else begin
			if (Stall)
				ID_instr <= ID_instr;
			else
				ID_instr <= IF_instr;
			ID_pc4   <= IF_pc4;
		end
	end

	// id 9
	assign ID_rd = ID_instr[15:11];

    // ------------------------------------------
    // ------------------------------------------

	// id 18
	assign address_select = (address[31:8] == BASE_ADDRESS);  // address decoding

	// id 16
	assign mem_offset     = address[7:2];  // drop 2 LSBs to get word offset
	always @(*) begin
		if (address_select == 1) begin
			case (mem_offset)
				5'd0  : IF_instr = {6'd35, 5'd0, 5'd2, 16'd4};            // lw $2, 4($0)     r2 = 1
				5'd1  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 2
				5'd2  : IF_instr = {6'd2, 26'd7};                         // jump to instruction 6
				5'd3  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 4 (should be skipped)
				5'd4  : IF_instr = 0;                                     // These are here just so our jump is more
				5'd5  : IF_instr = 0;                                     // visible
				5'd6  : IF_instr = 0;
				5'd7  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 4
				5'd8  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 8
				default IF_instr = 32'hxxxx;
			endcase
		end
	end

	// id 18
	assign address_select = (address[31:8] == BASE_ADDRESS);  // address decoding

	// id 16
	assign mem_offset     = address[7:2];  // drop 2 LSBs to get word offset
	always @(*) begin
		if (address_select == 1) begin
			case (mem_offset)
				5'd0  : IF_instr = {6'd35, 5'd0, 5'd2, 16'd4};            // lw $2, 4($0)     r2 = 1
				5'd1  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 2
				5'd2  : IF_instr = {6'd2, 26'd7};                         // jump to instruction 6
				5'd3  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 4 (should be skipped)
				5'd4  : IF_instr = 0;                                     // These are here just so our jump is more
				5'd5  : IF_instr = 0;                                     // visible
				5'd6  : IF_instr = 0;
				5'd7  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 4
				5'd8  : IF_instr = {6'd0, 5'd2, 5'd2, 5'd2, 5'd0, 6'd32}; // add $2, $2, $2   r2 = 8
				default IF_instr = 32'hxxxx;
			endcase
		end
	end

	// id 34
	always @(*) begin
		case (EX_Operation)
			3'b000 : EX_ALUOut = MuxA_out & EX_alub; // AND
			3'b001 : EX_ALUOut = MuxA_out | EX_alub; // OR
			3'b010 : EX_ALUOut = MuxA_out + EX_alub; // ADD
			3'b110 : EX_ALUOut = MuxA_out - EX_alub; // SUBTRACT
			3'b111 : if (MuxA_out < EX_alub) 
				EX_ALUOut = 32'd1; 
			else 
				EX_ALUOut = 32'd0; //SLT      
			default : EX_ALUOut = 32'hxxxxxxxx;
		endcase

		if (EX_ALUOut == 32'd0)
			EX_Zero = 1;
		else 
			EX_Zero = 0;
	end

	// id 19
	always @(*) begin   
		ID_rd1 = ID_rs + file_array;
		ID_rd1 = ID_rt + file_array;
	end


endmodule
