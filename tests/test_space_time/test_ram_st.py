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

    valid_clocks = [(o<n) for o in range(n+i)]

    tester.circuit.WE = True
    tester.circuit.RE = True
    clk = 0
    for k in range(2):
        for j in range(n+i):
            tester.print("clk: {}\n".format(clk))
            #tester.print("last: %d\n", tester.circuit.last)
            tester.print("inner read counter: %d\n", tester.circuit.ir)
            tester.print("inner write counter: %d\n", tester.circuit.iw)
            tester.circuit.WADDR = k
            tester.circuit.RADDR = k - 1
            tester.circuit.WDATA = j + k * (n+i)
            tester.eval()
            if k == 1 and valid_clocks[j]:
                tester.circuit.RDATA.expect(j)
            tester.step(2)
            clk += 1
    compile_and_run(tester)

def test_ram_st_TSeq_3_1_SSeq_2():
    no = 3
    io = 1
    ni = 2
    t = ST_TSeq(no, io, ST_SSeq(ni, ST_Int()))
    num_t = 2
    testcircuit = DefineRAM_ST(t, num_t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    valid_clocks = [(o<no) for o in range(no+io)]

    check_ram_st(no+io, num_t, tester, valid_clocks)

def test_ram_st_TSeq_3_1_SSeq_2_TSeq_2_2():
    no = 3
    io = 1
    ni = 2
    nii = 2
    iii = 2
    t = ST_TSeq(no, io, ST_SSeq(ni, ST_TSeq(nii, iii, ST_Int())))
    num_t = 2
    testcircuit = DefineRAM_ST(t, num_t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    valid_clocks = [(o < no) and (ii < nii) for o in range(no+io) for ii in range(nii+iii)]

    check_ram_st((no+io) * (nii+iii), num_t, tester, valid_clocks)


def check_ram_st(clocks, num_t, tester, valid_clocks):
    tester.circuit.WE = True
    tester.circuit.RE = True
    clk = 0
    for k in range(num_t):
        for j in range(clocks):
            tester.print("clk: {}\n".format(clk))
            # tester.print("last: %d\n", tester.circuit.last)
            tester.print("inner read counter: %d\n", tester.circuit.ir)
            tester.print("inner write counter: %d\n", tester.circuit.iw)
            tester.circuit.WADDR = k
            tester.circuit.RADDR = k - 1
            tester.circuit.WDATA = [2 * (j + k * clocks), 2 * (j + k * clocks) + 1]
            tester.eval()
            if k == 1 and valid_clocks[j]:
                tester.circuit.RDATA.expect([2 * j, 2 * j + 1])
            tester.step(2)
            clk += 1
    compile_and_run(tester)
