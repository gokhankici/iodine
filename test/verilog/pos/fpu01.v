module test(clk, opa, opb, fpu_op, out);
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
   // @annot{taint_source(fpu_op)}
   input wire clk, opa, opb, fpu_op;

   // @annot{taint_sink(out)}
   output reg out;

   reg opa_r, opb_r, fpu_op_r;
   reg sticky, sign_d;

   always @(posedge clk)
     opa_r <= opa;
   
   always @(posedge clk)
     opb_r <= opb;

   always @(posedge clk)
     fpu_op_r <= fpu_op;

   assign adj_op_tmp = opa_r | opb_r;

   always @(*)
     case(opa_r > opb_r)
       00:      sticky = 1'h0;
       01:      sticky =  adj_op_tmp[0]; 
       default: sticky = |adj_op_tmp[26:0];
     endcase

   assign signa            = opa_r[31];
   assign signb            = opb_r[31];
   assign fracta_n         = opa_r + opb_r + sticky + 1;
   assign fractb_n         = opa_r + opb_r + sticky + 2;
   assign fractb_lt_fracta = fractb_n > fracta_n;
   assign add              = !fpu_op_r[0];

   always @(*) begin
      case({signa, signb, add})
        3'b001:  sign_d = 0;
        3'b011:  sign_d = fractb_lt_fracta;
        3'b101:  sign_d = !fractb_lt_fracta;
        3'b111:  sign_d = 1;
        3'b000:  sign_d = fractb_lt_fracta;
        3'b010:  sign_d = 0;
        3'b100:  sign_d = 1;
        default: sign_d = !fractb_lt_fracta;
      endcase
   end

   always @(posedge clk)
     out <= sign_d;
   
endmodule
