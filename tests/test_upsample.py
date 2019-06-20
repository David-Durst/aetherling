from aetherling.modules.upsample import UpsampleSequential, UpsampleParallel
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
from mantle.common.countermod import CounterModM, Decode

def test_up_parallel():
    width = 5
    numOut = 2
    testVal = 3
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Array[numOut, Out(inType)]
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    upParallel = UpsampleParallel(numOut, inType)
    wire(upParallel.I, testcircuit.I)
    wire(testcircuit.O, upParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    sim.evaluate()
    sim.advance_cycle()
    sim.evaluate()
    for i in range(numOut):
        assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testVal

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


def test_up_sequential_rv_and_ce_flicker():
    width = 5
    num_out = 3
    time_per_element = 1
    num_valid_clocks = num_out * time_per_element
    num_clocks_per_iteration = num_valid_clocks * 12
    num_iterations = 2
    num_total_clocks = num_clocks_per_iteration * num_iterations
    test_val = 3
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Out(inType)
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    upSequential = UpsampleSequential(num_out, time_per_element, inType, has_ce=True)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)
    wire(testcircuit.ready_up, upSequential.ready_up)
    wire(testcircuit.valid_up, upSequential.valid_up)
    wire(testcircuit.ready_down, upSequential.ready_down)
    wire(testcircuit.valid_down, upSequential.valid_down)
    wire(testcircuit.CE, upSequential.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    valid_clks = [i for i in range(num_total_clocks) if i % 12 == 0]

    # only ready one element per num_out as outputs need to repeat num_out times
    ready_clks = []
    # multiply
    for element in range(num_out * num_iterations):
        for i_per_el in range(time_per_element):
            if element % num_out == 0:
                ready_clks.append(valid_clks[element + i_per_el])

    for clk in range(num_total_clocks):
        sim.set_value(testcircuit.valid_up, clk % 2 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 3 == 0, scope)
        sim.set_value(testcircuit.CE, clk % 4 == 0, scope)
        sim.set_value(testcircuit.I, int2seq(test_val + clk // num_clocks_per_iteration, width), scope)
        sim.evaluate()
        if clk in valid_clks:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == test_val + clk // num_clocks_per_iteration
        assert sim.get_value(testcircuit.valid_down, scope) == (clk in valid_clks)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk in ready_clks)
        sim.advance_cycle()
        sim.evaluate()

def test_up_sequential_multi_clock_elements():
    width = 5
    num_out = 3
    time_per_element = 2
    num_valid_clocks = num_out * time_per_element
    num_clocks_per_iteration = num_valid_clocks
    num_iterations = 2
    num_total_clocks = num_clocks_per_iteration * num_iterations
    test_vals = [3,4, 5, 6]
    read_test_val_idx = 0
    write_test_val_idx = 0
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Out(inType)
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    upSequential = UpsampleSequential(num_out, time_per_element, inType, has_ce=True)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)
    wire(testcircuit.ready_up, upSequential.ready_up)
    wire(testcircuit.valid_up, upSequential.valid_up)
    wire(testcircuit.ready_down, upSequential.ready_down)
    wire(testcircuit.valid_down, upSequential.valid_down)
    wire(testcircuit.CE, upSequential.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    valid_clks = [i for i in range(num_total_clocks)]

    # only ready one element per num_out as outputs need to repeat num_out times
    ready_clks = []
    # multiply
    for element in range(num_out * num_iterations):
        for i_per_el in range(time_per_element):
            if element % num_out == 0:
                ready_clks.append(valid_clks[element * time_per_element + i_per_el])

    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    sim.set_value(testcircuit.CE, True, scope)
    for clk in range(num_total_clocks):
        sim.set_value(testcircuit.I, int2seq(test_vals[write_test_val_idx], width), scope)
        sim.evaluate()
        if clk in valid_clks:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[read_test_val_idx]
        assert sim.get_value(testcircuit.valid_down, scope) == (clk in valid_clks)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk in ready_clks)
        sim.advance_cycle()
        sim.evaluate()

        if clk in valid_clks:
            # only use the inputs for the current input sequence
            read_test_val_idx += 1
            read_test_val_idx %= 2
            read_test_val_idx += 2 * ((clk+1) // num_clocks_per_iteration)
        if clk in ready_clks:
            write_test_val_idx += 1
            # after finishing all the test values, on last ready clock, will need to roll back to start so
            # don't overflow test_vals array
            write_test_val_idx %= len(test_vals)

def test_up_sequential_multi_clock_elements_flicker_rv_and_ce():
    width = 5
    num_out = 3
    time_per_element = 2
    num_valid_clocks = num_out * time_per_element
    clks_between_valid_clks = 12
    num_clocks_per_iteration = num_valid_clocks * clks_between_valid_clks
    num_iterations = 2
    num_total_clocks = num_clocks_per_iteration * num_iterations
    test_vals = [3,4,5,6]
    read_test_val_idx = 0
    write_test_val_idx = 0
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Out(inType)
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    upSequential = UpsampleSequential(num_out, time_per_element, inType, has_ce=True)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)
    wire(testcircuit.ready_up, upSequential.ready_up)
    wire(testcircuit.valid_up, upSequential.valid_up)
    wire(testcircuit.ready_down, upSequential.ready_down)
    wire(testcircuit.valid_down, upSequential.valid_down)
    wire(testcircuit.CE, upSequential.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    valid_clks = [i for i in range(num_total_clocks) if i % clks_between_valid_clks == 0]

    # only ready one element per num_out as outputs need to repeat num_out times
    ready_clks = []
    # multiply
    for element in range(num_out * num_iterations):
        for i_per_el in range(time_per_element):
            if element % num_out == 0:
                ready_clks.append(valid_clks[element * time_per_element + i_per_el])

    for clk in range(num_total_clocks):
        sim.set_value(testcircuit.valid_up, clk % 2 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 3 == 0, scope)
        sim.set_value(testcircuit.CE, clk % 4 == 0, scope)
        sim.set_value(testcircuit.I, int2seq(test_vals[write_test_val_idx], width), scope)
        sim.evaluate()
        if clk in valid_clks:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[read_test_val_idx]
        assert sim.get_value(testcircuit.valid_down, scope) == (clk in valid_clks)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk in ready_clks)
        sim.advance_cycle()
        sim.evaluate()

        if clk in valid_clks:
            # only use the inputs for the current input sequence
            read_test_val_idx += 1
            read_test_val_idx %= 2
            read_test_val_idx += 2 * ((clk+clks_between_valid_clks) // num_clocks_per_iteration)
        if clk in ready_clks:
            write_test_val_idx += 1
            # after finishing all the test values, on last ready clock, will need to roll back to start so
            # don't overflow test_vals array
            write_test_val_idx %= len(test_vals)
"""

width = 5
numElements = 1
testVal = 3
c = coreir.Context()
scope = Scope()
inType = Array[width, In(BitIn)]
#outType = Array[width, Out(BitIn)] #uncomment this line
#outType = Array[numElements, Out(inType)]
outType = Array[numElements, Out(Array[width, Bit])] #comment this line
args = ['I', inType, 'O', outType] + ClockInterface(False, False)

testcircuit = DefineCircuit('Test', *args)

#upParallel = UpParallel(numElements, inType)
#wire(upParallel.I, testcircuit.I)
#wire(testcircuit.O, upParallel.O)
#wire(testcircuit.O, testcircuit.I) # uncomment this line
wire(testcircuit.O[0], testcircuit.I) # comment this line


EndCircuit()

sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

#coreir_testcircuit = compile(testcircuit, context=c)

#c.run_passes(["rungenerators", "verifyconnectivity-onlyinputs-noclkrst",
                    #"wireclocks-coreir", "flatten", "flattentypes", "verifyconnectivity",
                    #"deletedeadinstances"])

#sim_testcircuit = coreir.SimulatorState(coreir_testcircuit)


#sim_testcircuit.set_value(["self.I"], bit_vector.BitVector(width, testVal).as_bool_list())
#sim = PythonSimulator(testcircuit)
sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
sim = CoreIRSimulator(testcircuit, testcircuit.CLK)
simulate(testcircuit, CoreIRSimulator)
"""