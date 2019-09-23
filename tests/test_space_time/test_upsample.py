from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_up_parallel():
    num_out = 2
    test_val = 3
    elem_t = ST_Int()


    up = DefineUp_S(num_out, elem_t, True)

    tester = fault.Tester(up, up.CLK)

    tester.circuit.valid_up = 1
    tester.circuit.I = test_val
    tester.eval()
    for i in range(num_out):
        tester.circuit.O[i].expect(test_val)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)
"""
def test_up_parallel_alternate_rv():
    width = 5
    numOut = 2
    testVal = 3
    num_clocks = 12
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Array[numOut, Out(inType)]
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    upParallel = UpsampleParallel(numOut, inType, has_ready_valid=True)
    wire(upParallel.I, testcircuit.I)
    wire(testcircuit.O, upParallel.O)
    wire(testcircuit.ready_up, upParallel.ready_up)
    wire(testcircuit.valid_up, upParallel.valid_up)
    wire(testcircuit.ready_down, upParallel.ready_down)
    wire(testcircuit.valid_down, upParallel.valid_down)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    for clk in range(num_clocks):
        sim.set_value(testcircuit.valid_up, clk % 3 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 2 == 0, scope)
        sim.evaluate()
        for i in range(numOut):
            assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testVal
        assert sim.get_value(testcircuit.valid_down, scope) == (clk % 3 == 0)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk % 2 == 0)
        sim.advance_cycle()
        sim.evaluate()

def test_modm_counter():
    width = 5
    numCycles = 4
    maxCounter = 4
    scope = Scope()
    args = ['O', Out(Bit)] + ClockInterface(False, False)
    testcircuit = DefineCircuit('TestModM', *args)
    counter = CounterModM(maxCounter, width, cout=False)
    decode = Decode(0, width)
    wire(decode.I, counter.O)
    wire(testcircuit.O, decode.O)
    EndCircuit()
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    for i in range(maxCounter * numCycles):
        sim.evaluate()
        sim.advance_cycle()

    assert sim.get_value(testcircuit.O, scope) == True


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
