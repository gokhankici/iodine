module mem32(clk, mem_read, mem_write, address, data_in, data_out);
   // Sources:
   // @annot{sanitize_glob(mem_read)}
   // @annot{taint_eq(mem_write)}
   // @annot{taint_eq(address)}
   // @annot{taint_eq(data_in)}
   // Sinks:
   // @annot{taint_source(data_out)}
   // @annot{taint_sink(data_out)}

   input         clk, mem_read, mem_write;
   input [31:0]  address, data_in;
   output [31:0] data_out;
   reg [31:0]    data_out;

   reg [31:0]    mem_array [0:31];

   always @(posedge clk) begin
    if (mem_read == 1'b1)
      data_out <= mem_array[address];
    else
      data_out <= 32'hxxxxxxxx;
   end

endmodule
