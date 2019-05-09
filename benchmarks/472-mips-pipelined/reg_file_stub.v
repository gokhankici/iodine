//-----------------------------------------------------------------------------
// Title         : Register File (32 32-bit registers)
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : reg_file.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   Behavioral model of the register file  used in the implementations of the MIPS
//   processor subset described in Ch. 5-6 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//   It implements the function specified in Fig 5-7 on p. 295 of COD3e.
//
//-----------------------------------------------------------------------------

module reg_file(clk, RegWrite, RN1, RN2, WN, RD1, RD2, WD);
   // source   : @annot{taint_source(RegWrite)}
   //            @annot{taint_source(RN1)}
   //            @annot{taint_source(RN2)}
   //            @annot{taint_source(WN)}
   //            @annot{taint_source(WD)}

   // sink     : @annot{taint_sink(RD1)}
   //            @annot{taint_sink(RD2)}

   // @annot{not_sanitize(file_array)}
   input         clk;
   input         RegWrite;
   input  [4:0]  RN1, RN2, WN;
   input  [31:0] WD;
   output [31:0] RD1, RD2;

   reg    [31:0] RD1, RD2;
   reg    [31:0] file_array [31:1];

   always @(*) begin   
	   RD1 = RN1 + file_array;
	   RD2 = RN2 + file_array;
   end

endmodule

