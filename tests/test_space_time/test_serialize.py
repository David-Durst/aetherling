from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run, wrap_module_with_top
from aetherling.space_time.modules.serialize import DefineSerialize

def test_serialize_basic():
    n = 5
    i = 0
    inputs = list(range(n))
    num_iterations = 2
    t = ST_Int()

    ser = DefineSerialize(n, i, t)

    tester = fault.Tester(ser, ser.CLK)

    tester.circuit.valid_up = 1
    tester.circuit.I = inputs
    for clk in range(num_iterations*(n+i)+1):
        pipelined_clk = clk - 1
        tester.print("clk: {}\n".format(clk))
        tester.eval()
        tester.step(2)
        if pipelined_clk >= 0:
            tester.circuit.valid_down.expect(1)
            tester.circuit.O.expect(inputs[pipelined_clk % n])
    compile_and_run(tester)

def test_serialize_with_invalid():
    n = 5
    i = 1
    inputs = list(range(n))
    wrong_inputs = [i+3 for i in range(n)]
    num_iterations = 2
    t = ST_Int()

    ser = DefineSerialize(n, i, t)

    tester = fault.Tester(ser, ser.CLK)

    tester.circuit.valid_up = 1
    for clk in range(num_iterations*(n+i)):
        pipelined_clk = clk - 1
        if clk % (n+i) == 0:
            tester.circuit.I = inputs
        else:
            tester.circuit.I = wrong_inputs
        tester.print("clk: {}\n".format(clk))
        tester.step(2)
        if pipelined_clk >= 0:
            tester.circuit.valid_down.expect(1)
            if pipelined_clk % (n+i) < n:
                tester.circuit.O.expect(inputs[pipelined_clk % (n+i)])
    compile_and_run(tester)

def test_serialize_multiple_clocks():
    no = 2
    io = 0
    ni = 2
    ii = 1
    inputs = [[1,2], [3,4]]
    wrong_inputs = [5,6]
    outputs = [1,3,2,4]
    num_iterations = 2
    t = ST_TSeq(ni, ii, ST_Int())

    ser = DefineSerialize(no, io, t)

    tester = fault.Tester(ser, ser.CLK)

    tester.circuit.valid_up = 1
    output_counter = 0
    for clk in range(num_iterations*(no+io)*t.time()):
        pipelined_clk = clk - 1
        if clk % ((no+io)*(ni+ii)) < ni:
            tester.circuit.I = inputs[clk % (no+io)]
        else:
            tester.circuit.I = wrong_inputs
        tester.print("clk: {}\n".format(clk))
        tester.step(2)
        if pipelined_clk >= 0:
            tester.circuit.valid_down.expect(1)
            if pipelined_clk in [0,1,3,4,6,7,9,10]:
                tester.circuit.O.expect(outputs[output_counter])
                output_counter += 1
                output_counter = output_counter % 4
    compile_and_run(tester)
