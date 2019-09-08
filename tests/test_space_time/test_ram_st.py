from aetherling.space_time.type_helpers import *
from aetherling.space_time.ram_st import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_ram_st_TSeq_3_1():
    n = 3
    i = 1
    t = ST_TSeq(n, i, ST_Int())
    num = 2
    testcircuit = DefineRAM_ST(t, num)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    valid_clocks = [o for o in range(n+i)]

    tester.circuit.WE = True
    tester.circuit.RE = True
    clk = 0
    for k in range(2):
        for j in range(n+i):
            tester.print("clk: {}\n".format(clk))
            #tester.print("last: %d\n", tester.circuit.last)
            tester.circuit.WADDR = k
            tester.circuit.RADDR = k - 1
            tester.circuit.WDATA = j + k * (n+i)
            tester.eval()
            if k == 1 and (j in valid_clocks):
                tester.circuit.RDATA.expect(j)
            tester.step(2)
            clk += 1
    compile_and_run(tester)
