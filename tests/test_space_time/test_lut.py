from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_lut_basic():
    num_in = 4
    num_iterations = 2
    test_vals = (2,4,6,8)
    out_type = ST_TSeq(num_in, 0, ST_Int())

    const = DefineConst(out_type, test_vals, has_valid=True)

    tester = fault.Tester(const, const.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for j in range(num_in):
            tester.print("clk: {}\n".format(i*num_iterations + j))
            tester.eval()
            tester.circuit.valid_down.expect(1)
            tester.circuit.O.expect(test_vals[j])
            tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_lut_parallel():
    no = 4
    io = 0
    ni = 2
    num_iterations = 2
    test_vals = ((1,2),(3,4),(5,6),(7,8))
    out_type = ST_TSeq(no, io, ST_SSeq(ni, ST_Int()))

    const = DefineConst(out_type, test_vals, has_valid=True)

    tester = fault.Tester(const, const.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for j in range(no):
            tester.print("clk: {}\n".format(i*num_iterations + j))
            tester.eval()
            tester.circuit.valid_down.expect(1)
            tester.circuit.O.expect(list(test_vals[j]))
            tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_lut_invalids():
    no = 4
    io = 1
    ni = 2
    num_iterations = 2
    test_vals = ((1,2),(3,4),(5,6),(7,8),(0,0))
    out_type = ST_TSeq(no, io, ST_SSeq(ni, ST_Int()))

    const = DefineConst(out_type, test_vals, has_valid=True)

    tester = fault.Tester(const, const.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for j in range(no+io):
            tester.print("clk: {}\n".format(i*num_iterations + j))
            tester.eval()
            tester.circuit.valid_down.expect(1)
            if j < no:
                tester.circuit.O.expect(list(test_vals[j]))
            tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)
