module mult16_2reg (a, b, p, rst, ce, CLK);
  input signed [7:0] a;
  input signed [7:0] b;
  input CLK;
  input rst;
  input ce;
  output [15:0] p;

  reg [15:0] p;
  reg [7:0] a1;
  reg [7:0] b1;
  wire [15:0] p1;

  assign p1 = a1*b1;

  always @(posedge CLK)
    if (rst == 1'b1)
      begin
        a1 <= 0;
        b1 <= 0;
        p <= 0;
      end
    else if (ce == 1'b1)
      begin
        a1 <= a;
        b1 <= b;
        p <= p1;
      end
endmodule

module mult16_4reg (a, b, p, rst, ce, CLK);
  input signed [7:0] a;
  input signed [7:0] b;
  input CLK;
  input rst;
  input ce;
  output [15:0] p;

  reg [15:0] p_0;
  reg [15:0] p_1;
  reg [15:0] p_2;
  reg [7:0] a1;
  reg [7:0] b1;
  wire [15:0] p;


  always @(posedge CLK)
    if (rst == 1'b1)
      begin
        a1 <= 0;
        b1 <= 0;
        p_0 <= 0;
        p_1 <= 0;
        p_2 <= 0;
      end
    else if (ce == 1'b1)
      begin
        a1 <= a;
        b1 <= b;
        p_0 <= a1*b1;
        p_1 <= p_0;
        p_2 <= p_1;
      end
  assign p = p_2;
endmodule

