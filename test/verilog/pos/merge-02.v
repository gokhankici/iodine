module test(clk, opa, opb, out);
   input wire clk;
   // @annot{taint_source(opa)}
   // @annot{taint_source(opb)}
   input wire [31:0] opa, opb;

   // @annot{taint_sink(out)}
   output reg [31:0] out;

   reg opa_r, opb_r;
   reg sticky;
   reg y;
   
   always @(posedge clk)
     opa_r <= opa;
   
   always @(posedge clk)
     opb_r <= opb;

   assign signa = opa_r[31];
   assign signb = opb_r[31];

   always @(*) begin
      sticky = opa_r + opb_r;
   end 
   
   always @(*) begin
      case({signa, signb})
        2'b00:   y = 0;
        2'b01:   y = sticky;
        default: y = ! sticky;
      endcase
   end

   always @(posedge clk)
     out <= y;

endmodule
