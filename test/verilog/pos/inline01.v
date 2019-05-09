module top(clk,x1,x2,y1,y2);
    input clk;
    input  wire x1;
    input  wire x2;
    output wire y1;
    output reg  y2;

    // @annot{taint_source(x1)}
    // @annot{taint_source(x2)}
    // @annot{taint_sink(y1)}
    // @annot{taint_sink(y2)}

    reg  top_reg;
    wire top_wire;

    sub1 SUB1(clk, x1, top_reg, y1, top_wire);

    always @(posedge clk) begin
        y2 <= x2; 
    end

endmodule // top

module sub1(clk,x1,x2,y1,y2);
    input clk;
    input  wire x1; // top.x1
    input  wire x2; // top.top_reg
    output wire y1; // top.y1
    output reg  y2; // top.top_wire

    // @annot{taint_source(x1)}
    // @annot{taint_source(x2)}
    // @annot{taint_sink(y1)}
    // @annot{taint_sink(y2)}

    reg  sub1_reg;
    wire sub1_wire;

    sub2 SUB2(clk, x1, sub1_reg, y1, sub1_wire);

    always @(posedge clk) begin
        y2 <= x2; 
    end

endmodule // top


module sub2(clk,x1,x2,y1,y2);
    input clk;
    input  wire x1; // sub1.x1
    input  wire x2; // sub1.sub1_reg
    output wire y1; // sub1.y1
    output reg  y2; // sub1.sub1_wire

    // @annot{taint_source(x1)}
    // @annot{taint_source(x2)}
    // @annot{taint_sink(y1)}
    // @annot{taint_sink(y2)}

    assign y1 = x1;    

    always @(posedge clk) begin
        y2 <= x2; 
    end

endmodule // top