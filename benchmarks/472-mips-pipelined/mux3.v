module mux3(sel, a, b, c, y);
  // @annot{taint_source(y)}
  // @annot{taint_sink(y)}

  // @annot{sanitize_glob(sel)}
  // @annot{taint_eq(a)}
  // @annot{taint_eq(b)}
  // @annot{taint_eq(c)}
   parameter bitwidth=32;

   input   [1:0]          sel;
   input   [bitwidth-1:0] a, b, c;
   output  [bitwidth-1:0] y;
   reg     [bitwidth-1:0] y;

   always @(*) begin
      case (sel)
        2'b00   :  y = a;
        2'b01   :  y = b;
        2'b10   :  y = c;
      endcase
   end
endmodule
