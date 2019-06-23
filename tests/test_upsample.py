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
    ce_period = 2
    ready_down_period = 3
    valid_up_period = 4
    num_iterations = 2
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

    for i in range(num_iterations):
        parts_of_elements_emitted = 0
        elements_emitted = 0
        clk = -1
        while elements_emitted < num_out:
            clk += 1
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            if clk % valid_up_period == 0:
                sim.set_value(testcircuit.I, int2seq(test_val + i, width), scope)
            # ensure that can change input on not valid_up clock and not affect anything
            else:
                sim.set_value(testcircuit.I, int2seq(0, width), scope)
            sim.evaluate()
            # check ready
            if clk % ce_period == 0 and clk % ready_down_period == 0 and elements_emitted == 0:
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False
            # check valid
            if clk % ce_period == 0 and (clk % valid_up_period == 0 or elements_emitted > 0):
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_val + i
                assert sim.get_value(testcircuit.valid_down, scope) == True
            else:
                assert sim.get_value(testcircuit.valid_down, scope) == False
            # increment counters each time circuit executes
            if clk % ce_period == 0 and clk % ready_down_period == 0 and \
                ((clk % valid_up_period == 0 and elements_emitted < 1) or elements_emitted >= 1):
                parts_of_elements_emitted += 1
                if parts_of_elements_emitted % time_per_element == 0:
                    elements_emitted += 1
            sim.advance_cycle()
            sim.evaluate()

def test_up_sequential_multi_clock_elements():
    width = 5
    num_out = 3
    time_per_element = 2
    num_iterations = 2
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

    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    sim.set_value(testcircuit.CE, True, scope)
    for i in range(num_iterations):
        parts_of_elements_emitted = 0
        elements_emitted = 0
        clk = -1
        while elements_emitted < num_out:
            clk += 1
            sim.set_value(testcircuit.I, int2seq(test_vals[write_test_val_idx + (time_per_element * i)], width), scope)
            sim.evaluate()
            assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[read_test_val_idx + (time_per_element * i)]
            assert sim.get_value(testcircuit.valid_down, scope) == True
            if elements_emitted == 0:
                assert sim.get_value(testcircuit.ready_up, scope) == True
            sim.advance_cycle()
            sim.evaluate()

            read_test_val_idx += 1
            read_test_val_idx %= time_per_element
            if elements_emitted == 0:
                write_test_val_idx += 1
                # after finishing all the test values, on last ready clock, will need to roll back to start so
                # don't overflow test_vals array
                write_test_val_idx %= time_per_element

            # since ready, valid, and CE in always true, always increment elements counters
            parts_of_elements_emitted += 1
            if parts_of_elements_emitted % time_per_element == 0:
                elements_emitted += 1

def test_up_sequential_multi_clock_elements_flicker_rv_and_ce():
    width = 5
    num_out = 3
    time_per_element = 2
    ce_period = 2
    ready_down_period = 3
    valid_up_period = 4
    num_iterations = 2
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

    for i in range(num_iterations):
        parts_of_elements_emitted = 0
        elements_emitted = 0
        clk = -1
        while elements_emitted < num_out:
            clk += 1
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            if clk % valid_up_period == 0:
                sim.set_value(testcircuit.I, int2seq(test_vals[write_test_val_idx + (time_per_element * i)], width), scope)
            # ensure that can change input on not valid_up clock and not affect anything
            else:
                sim.set_value(testcircuit.I, int2seq(0, width), scope)
            sim.evaluate()

            # check ready
            if clk % ce_period == 0 and clk % ready_down_period == 0 and elements_emitted == 0:
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False

            # check valid
            if clk % ce_period == 0 and (clk % valid_up_period == 0 or elements_emitted > 0):
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[read_test_val_idx + (time_per_element * i)]
                assert sim.get_value(testcircuit.valid_down, scope) == True
            else:
                assert sim.get_value(testcircuit.valid_down, scope) == False

            # increment counters each time circuit executes
            if clk % ce_period == 0 and clk % ready_down_period == 0 and \
                    ((clk % valid_up_period == 0 and elements_emitted < 1) or elements_emitted >= 1):
                read_test_val_idx += 1
                read_test_val_idx %= time_per_element
                if elements_emitted == 0:
                    write_test_val_idx += 1
                    # after finishing all the test values, on last ready clock, will need to roll back to start so
                    # don't overflow test_vals array
                    write_test_val_idx %= time_per_element

                parts_of_elements_emitted += 1

                if parts_of_elements_emitted % time_per_element == 0:
                    elements_emitted += 1

            sim.advance_cycle()
            sim.evaluate()
