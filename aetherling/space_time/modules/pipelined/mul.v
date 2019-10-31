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