from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_up_s():
    num_out = 2
    test_val = 3
    elem_t = ST_Int()


    up = DefineUp_S(num_out, elem_t, True)

    tester = fault.Tester(up, up.CLK)

    tester.circuit.valid_up = 1
    tester.circuit.I[0] = test_val
    tester.eval()
    for i in range(num_out):
        tester.circuit.O[i].expect(test_val)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_up_s_sseq():
    n = 2
    num_out = 2
    test_vals = [3,5]
    elem_t = ST_SSeq(n, ST_Int())


    up = DefineUp_S(num_out, elem_t, True)

    tester = fault.Tester(up, up.CLK)

    tester.circuit.valid_up = 1
    for i, v in enumerate(test_vals):
        tester.circuit.I[0][i] = v
    tester.eval()
    for i in range(num_out):
        for j, v in enumerate(test_vals):
            tester.circuit.O[i][j].expect(v)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_up_t_v_always_true_no_ce():
    num_out = 4
    test_val = 3
    elem_t = ST_Int()

    up = DefineUp_T(num_out, 0, elem_t, has_valid=True)

    tester = fault.Tester(up, up.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_out):
        if i == 0:
            tester.circuit.I = test_val
        else:
            tester.circuit.I = test_val + 1
        tester.eval()
        tester.circuit.O.expect(test_val)
        tester.circuit.valid_down.expect(1)
        tester.step(2)
    compile_and_run(tester)

def test_up_t_invalids_v_always_true_no_ce():
    iterations = 2
    num_out = 4
    invalids = 2
    test_val = 3
    elem_t = ST_Int()

    up = DefineUp_T(num_out, invalids, elem_t, has_valid=True)

    tester = fault.Tester(up, up.CLK)

    tester.circuit.valid_up = 1
    for i in range(iterations):
        for j in range(num_out + invalids):
            if j == 0:
                tester.circuit.I = test_val + i
            else:
                tester.circuit.I = test_val + 4
            tester.eval()
            if j < num_out:
                tester.circuit.O.expect(test_val + i)
            tester.circuit.valid_down.expect(1)
            tester.step(2)
    compile_and_run(tester)

def test_up_t_invalids_v_delated_true_ce():
    iterations = 2
    delay = 5
    num_out = 4
    invalids = 2
    test_val = 3
    elem_t = ST_Int()

    up = DefineUp_T(num_out, invalids, elem_t, has_ce=True, has_valid=True)

    tester = fault.Tester(up, up.CLK)

    tester.circuit.valid_up = 0
    tester.circuit.CE = 1
    tester.circuit.I = test_val + 4
    for i in range(delay):
        tester.step(2)
    tester.circuit.valid_up = 1
    for i in range(iterations):
        for j in range(num_out + invalids):
            if j == 0:
                tester.circuit.I = test_val + i
            else:
                tester.circuit.I = test_val + 4
            tester.eval()
            if j < num_out:
                tester.circuit.O.expect(test_val + i)
            tester.circuit.valid_down.expect(1)
            tester.step(2)
    compile_and_run(tester)
"""

def test_up_sequential_rv_always_true_no_ce():
    width = 5
    num_out = 3
    num_clocks = num_out * 4
    test_val = 3
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Out(inType)
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    upSequential = UpsampleSequential(num_out, 1, inType)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)
    wire(testcircuit.ready_up, upSequential.ready_up)
    wire(testcircuit.valid_up, upSequential.valid_up)
    wire(testcircuit.ready_down, upSequential.ready_down)
    wire(testcircuit.valid_down, upSequential.valid_down)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(test_val, width), scope)
    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    sim.evaluate()

    for i in range(num_clocks):
        assert seq2int(sim.get_value(testcircuit.O, scope)) == test_val
        assert sim.get_value(testcircuit.ready_up, scope) == (i % num_out == 0)
        assert sim.get_value(testcircuit.valid_down, scope) == True
        sim.advance_cycle()
        sim.evaluate()
"""
