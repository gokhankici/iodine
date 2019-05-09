//-----------------------------------------------------------------------------
// Title         : ALU Behavioral Model
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : add32.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   Behavioral model of 32-bit adder  used in the implementations of the MIPS processor
//   subset described in Ch. 5-6 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//-----------------------------------------------------------------------------

module add32(a, b, result);
   // Sources:
   // @annot{taint_source(a)}
   // @annot{taint_source(b)}
   // Sinks:
   // @annot{taint_sink(result)}
   input [31:0] a, b;
   output [31:0] result;

   assign result = a + b;
endmodule

