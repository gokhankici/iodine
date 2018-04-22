// this example currently does not go through
// since our transition relation does not guarantee that
// always blocks 2 & 3 execute in lock step

module test(clk);
   input wire clk;
   wire       choice;
   reg        x;
   reg        y;
   reg        z;
   reg        t;

   // @annot{taint_source(x)}
   // @annot{taint_sink(t)}

   // @annot{sanitize_glob(x)}
   // @annot{sanitize(y,z,t)}

   // @annot{sanitize_mod(reg_file, val)}
   reg_file REG_FILE(clk, 0, choice);

   always @(posedge clk) //id 1
     if (choice)
       t <= y;
     else
       t <= z;

   always @(posedge clk) //id 2
     y <= x * 2;

   always @(posedge clk) //id 3
     z <= x * 3;

endmodule

module reg_file(clk, regNo, val);
   input wire clk;
   input wire regNo;
   output reg val;

   // 10 x 32-bit registers
   reg [31:0] reg_array [9:0];

   always @(posedge clk ) begin
      val <= reg_array[regNo];
   end
endmodule