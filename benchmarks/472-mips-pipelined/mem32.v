//-----------------------------------------------------------------------------
// Title         : Read-Write Memory (RAM)
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : mem32.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   Behavioral model of a read-write memory used in the implementations of the MIPS
//   processor subset described in Ch. 5-6 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//   Initial contents are specified in the "initial" block, which can be changed
//   as desired.
//
//-----------------------------------------------------------------------------

module mem32(clk, mem_read, mem_write, address, data_in, data_out);
   // Sources:
   // @annot{taint_source(mem_read)}
   // @annot{taint_source(mem_write)}
   // @annot{taint_source(address)}
   // @annot{taint_source(data_in)}
   // Sinks:
   // @annot{taint_sink(data_out)}

   input         clk, mem_read, mem_write;
   input [31:0]  address, data_in;
   output [31:0] data_out;
   reg [31:0]    data_out;

   parameter BASE_ADDRESS = 25'd0; // address that applies to this memory - change if desired

   reg [31:0]    mem_array [0:31];

   // @annot{sanitize(data_out)}

   wire [4:0]    mem_offset;
   wire          address_select;

   assign mem_offset = address[6:2];  // drop 2 LSBs to get word offset

   assign address_select = (address[31:7] == BASE_ADDRESS);  // address decoding

   always @(*) begin 
      if (mem_read == 1'b1 && address_select == 1'b1) begin
         if ((address % 4) != 0) begin
           $display($time, " rom32 error: unaligned address %d", address);
         end
         data_out = mem_array[mem_offset];
         $display($time, " reading data: Mem[%h] => %h", address, data_out);
      end
      else 
        data_out = 32'hxxxxxxxx;
   end

   // for WRITE operations
   always @(posedge clk) begin
      if (mem_write == 1'b1 && address_select == 1'b1) begin
         $display($time, " writing data: Mem[%h] <= %h", address,data_in);
         mem_array[mem_offset] <= data_in;
      end
   end

   // initialize with some arbitrary values

   integer i;
   initial begin
      for (i=0; i<7; i=i+1)
	mem_array[i] = i;
   end

endmodule
