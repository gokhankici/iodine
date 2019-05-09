//-----------------------------------------------------------------------------
// Title         : 2-1 multiplexer
// Project       : ECE 313 - Computer Organization
//-----------------------------------------------------------------------------
// File          : mux2.v
// Author        : John Nestor  <nestorj@lafayette.edu>
// Organization  : Lafayette College
// 
// Created       : October 2002
// Last modified : 7 January 2005
//-----------------------------------------------------------------------------
// Description :
//   Parameterized 2-1 mux  used in the implementations of the MIPS processor
//   subset described in Ch. 5-6 of "Computer Organization and Design, 3rd ed."
//   by David Patterson & John Hennessey, Morgan Kaufmann, 2004 (COD3e).  
//
//-----------------------------------------------------------------------------

module mux2( sel, a, b, y );
   parameter bitwidth=32;

   input  sel;
   input [bitwidth-1:0] a, b;
   output [bitwidth-1:0] y;

   assign y = sel ? b : a;
endmodule
