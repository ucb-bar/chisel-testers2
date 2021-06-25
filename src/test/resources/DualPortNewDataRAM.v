/*
In order to be able to make Quartus freely use M20K or MLAB RAM we need to have the same mode for all 3 read-during-write modes.
One solution could be to have "Port B RDW Mode and Port B RDW Mode" to have "old data", but I have found no way to set this.
But it is possible to get all 3 modes to have "new data". The code below will infer RAM that will fit into both M20K and MLAB RAM and get rid of the warning:
(Note the blocking assignments...)
*/

module DualPortNewDataRAM(
    input clk,
    input ram_we,
    input [ADDR - 1:0] ram_wr_addr,
    input [WIDTH - 1 : 0] ram_din,
    input [ADDR - 1:0] ram_rd_addr,
    output [WIDTH - 1 : 0] ram_dout,
    input ram_re,
    input [ADDR - 1:0] ram_rd_addr2,
    output [WIDTH - 1 : 0] ram_dout2,
    input ram_re2
);
	parameter WIDTH = 512;
 	parameter ADDR = 16;

    reg [WIDTH - 1 : 0] my_ram [0: (1 << ADDR) - 1] /* verilator public */;

    assign ram_dout = my_ram[ram_rd_addr];
    assign ram_dout2 = my_ram[ram_rd_addr2];

    always @(posedge clk) begin
        if (ram_we) begin
            my_ram[ram_wr_addr] = ram_din;
        end
    end
endmodule
