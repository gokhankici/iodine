module test(clk, opa, opb, fpu_op, out);
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
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
       02:      sticky = |adj_op_tmp[01:0];
       03:      sticky = |adj_op_tmp[02:0];
       04:      sticky = |adj_op_tmp[03:0];
       05:      sticky = |adj_op_tmp[04:0];
       06:      sticky = |adj_op_tmp[05:0];
       07:      sticky = |adj_op_tmp[06:0];
       08:      sticky = |adj_op_tmp[07:0];
       09:      sticky = |adj_op_tmp[08:0];
       10:      sticky = |adj_op_tmp[09:0];
       11:      sticky = |adj_op_tmp[10:0];
       12:      sticky = |adj_op_tmp[11:0];
       13:      sticky = |adj_op_tmp[12:0];
       14:      sticky = |adj_op_tmp[13:0];
       15:      sticky = |adj_op_tmp[14:0];
       16:      sticky = |adj_op_tmp[15:0];
       17:      sticky = |adj_op_tmp[16:0];
       18:      sticky = |adj_op_tmp[17:0];
       19:      sticky = |adj_op_tmp[18:0];
       20:      sticky = |adj_op_tmp[19:0];
       21:      sticky = |adj_op_tmp[20:0];
       22:      sticky = |adj_op_tmp[21:0];
       23:      sticky = |adj_op_tmp[22:0];
       24:      sticky = |adj_op_tmp[23:0];
       25:      sticky = |adj_op_tmp[24:0];
       26:      sticky = |adj_op_tmp[25:0];
       default: sticky = |adj_op_tmp[26:0];
     endcase

   assign signa            = opa_r[31];
   assign signb            = opb_r[31];
   assign fracta_n         = opa_r + opb_r + sticky + 1;
   assign fractb_n         = opa_r + opb_r + sticky + 2;
   assign fractb_lt_fracta = fractb_n > fracta_n;
   assign add              = !fpu_op_r[0];

   always @(*)
      case({signa, signb, add})
	3'b001:   sign_d = 0;
	3'b011:   sign_d = fractb_lt_fracta;
	3'b101:   sign_d = !fractb_lt_fracta;
	3'b111:   sign_d = 1;
	3'b000:   sign_d = fractb_lt_fracta;
	3'b010:   sign_d = 0;
	3'b100:   sign_d = 1;
	default:  sign_d = !fractb_lt_fracta;
      endcase

   always @(posedge clk)
     out <= sign_d;

endmodule
