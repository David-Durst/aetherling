from aetherling.modules.serializer import Serializer, Deserializer
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import GetCoreIRModule
import magma
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *

def test_serializer_rv_ce_always_true():
    width = 5
    numIn = 5
    numIterations = 2
    scope = Scope()
    inType = In(Array[numIn, Array[width, BitIn]])
    outType = Out(Array[width, Bit])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('Test_Serializer', *args)

    serializer = Serializer(numIn, 1, inType.T, has_ce=True)
    wire(serializer.I, testcircuit.I)
    wire(testcircuit.O, serializer.O)
    wire(testcircuit.ready_up, serializer.ready_up)
    wire(testcircuit.valid_up, serializer.valid_up)
    wire(testcircuit.ready_down, serializer.ready_down)
    wire(testcircuit.valid_down, serializer.valid_down)
    wire(testcircuit.CE, serializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIterations):
        sim.set_value(testcircuit.CE, True, scope)
        sim.set_value(testcircuit.valid_up, True, scope)
        sim.set_value(testcircuit.ready_down, True, scope)

        for j in range(numIn):
            sim.set_value(testcircuit.I[j], int2seq(i * numIn + j, width), scope)

        sim.evaluate()

        for j in range(numIn):
            assert sim.get_value(testcircuit.ready_up, scope) == (j == 0)
            assert sim.get_value(testcircuit.valid_down, scope) == True
            assert seq2int(sim.get_value(testcircuit.O, scope)) == i * numIn + j
            sim.evaluate()
            sim.advance_cycle()
            sim.evaluate()
            # this ensures that the output is not dpeendent on the input after first clock
            # as changing inputs to outputs that would be invalid
            for j in range(numIn):
                sim.set_value(testcircuit.I[j], int2seq(0, width), scope)

def test_serializer_rv_and_ce_flicker():
    width = 5
    num_in = 5
    time_per_element = 1
    ce_period = 2
    ready_down_period = 3
    valid_up_period = 4
    num_iterations = 2
    scope = Scope()
    inType = In(Array[num_in, Array[width, BitIn]])
    outType = Out(Array[width, Bit])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('Test_Serializer', *args)

    serializer = Serializer(num_in, time_per_element, inType.T, has_ce=True)
    wire(serializer.I, testcircuit.I)
    wire(testcircuit.O, serializer.O)
    wire(testcircuit.ready_up, serializer.ready_up)
    wire(testcircuit.valid_up, serializer.valid_up)
    wire(testcircuit.ready_down, serializer.ready_down)
    wire(testcircuit.valid_down, serializer.valid_down)
    wire(testcircuit.CE, serializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(num_iterations):
        for j in range(num_in):
            sim.set_value(testcircuit.I[j], int2seq(i * num_in + j, width), scope)

        parts_of_elements_emitted = 0
        elements_emitted = 0
        clk = -1
        while elements_emitted < num_in:
            clk += 1
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            # ensure that can change input after first element emitted and not affect anything
            # as serializer memory set
            if elements_emitted == 1:
                for j in range(num_in):
                    sim.set_value(testcircuit.I[j], int2seq(0, width), scope)
            sim.evaluate()
            # check ready
            if clk % ce_period == 0 and clk % ready_down_period == 0 and elements_emitted == 0:
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False
            # check valid
            if clk % ce_period == 0 and (clk % valid_up_period == 0 or elements_emitted > 0):
                assert seq2int(sim.get_value(testcircuit.O, scope)) == i * num_in + elements_emitted
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

def test_serializer_multi_clock_elements():
    width = 5
    num_in = 3
    time_per_element = 2
    num_iterations = 2
    scope = Scope()
    inType = In(Array[num_in, Array[width, BitIn]])
    outType = Out(Array[width, Bit])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    serializer = Serializer(num_in, time_per_element, inType.T, has_ce=True)
    wire(serializer.I, testcircuit.I)
    wire(testcircuit.O, serializer.O)
    wire(testcircuit.ready_up, serializer.ready_up)
    wire(testcircuit.valid_up, serializer.valid_up)
    wire(testcircuit.ready_down, serializer.ready_down)
    wire(testcircuit.valid_down, serializer.valid_down)
    wire(testcircuit.CE, serializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    sim.set_value(testcircuit.CE, True, scope)
    for i in range(num_iterations):
        parts_of_elements_emitted = 0
        elements_emitted = 0
        clk = -1
        while elements_emitted < num_in:
            clk += 1
            if elements_emitted == 0:
                for j in range(num_in):
                    # note: parts_of_elements_emitted only ever 0 or 1 here as this code only executes if
                    # elements_emitted == 0
                    sim.set_value(testcircuit.I[j], int2seq(i * (num_in + time_per_element) + \
                                                            j * time_per_element + parts_of_elements_emitted, width), scope)
            sim.evaluate()
            assert seq2int(sim.get_value(testcircuit.O, scope)) == i * (num_in + time_per_element) + \
                   elements_emitted * time_per_element + (parts_of_elements_emitted % time_per_element)

            # valid_down and ready_up should always be true since valid_up, ready_down, and CE always true
            assert sim.get_value(testcircuit.valid_down, scope) == True
            if elements_emitted == 0:
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False

            sim.advance_cycle()
            sim.evaluate()

            # since ready, valid, and CE in always true, always increment elements counters
            parts_of_elements_emitted += 1
            if parts_of_elements_emitted % time_per_element == 0:
                elements_emitted += 1


def test_serializer_multi_clock_elements_flicker_rv_and_ce():
    width = 5
    num_in = 3
    time_per_element = 2
    ce_period = 2
    ready_down_period = 3
    valid_up_period = 4
    num_iterations = 2
    scope = Scope()
    inType = In(Array[num_in, Array[width, BitIn]])
    outType = Out(Array[width, Bit])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    serializer = Serializer(num_in, time_per_element, inType.T, has_ce=True)
    wire(serializer.I, testcircuit.I)
    wire(testcircuit.O, serializer.O)
    wire(testcircuit.ready_up, serializer.ready_up)
    wire(testcircuit.valid_up, serializer.valid_up)
    wire(testcircuit.ready_down, serializer.ready_down)
    wire(testcircuit.valid_down, serializer.valid_down)
    wire(testcircuit.CE, serializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    for i in range(num_iterations):
        for j in range(num_in):
            sim.set_value(testcircuit.I[j], int2seq(i * num_in + j, width), scope)

        parts_of_elements_emitted = 0
        elements_emitted = 0
        clk = -1
        while elements_emitted < num_in:
            clk += 1
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)

            if elements_emitted == 0:
                for j in range(num_in):
                    # note: parts_of_elements_emitted only ever 0 or 1 here as this code only executes if
                    # elements_emitted == 0
                    sim.set_value(testcircuit.I[j], int2seq(i * (num_in + time_per_element) + \
                                                            j * time_per_element + parts_of_elements_emitted, width), scope)
            sim.evaluate()

            # check ready
            if clk % ce_period == 0 and clk % ready_down_period == 0 and elements_emitted == 0:
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False

            # check valid
            if clk % ce_period == 0 and (clk % valid_up_period == 0 or elements_emitted > 0):
                assert seq2int(sim.get_value(testcircuit.O, scope)) == i * (num_in + time_per_element) + \
                       elements_emitted * time_per_element + (parts_of_elements_emitted % time_per_element)
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

def test_deserializer_rv_ce_always_true():
    width = 5
    num_in = 5
    num_iterations = 2
    scope = Scope()
    inType = In(Array[width, Bit])
    outType = Out(Array[num_in, Array[width, BitIn]])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('Test_Deserializer', *args)

    deserializer = Deserializer(num_in, 1, inType, has_ce=True)
    wire(deserializer.I, testcircuit.I)
    wire(testcircuit.O, deserializer.O)
    wire(testcircuit.ready_up, deserializer.ready_up)
    wire(testcircuit.valid_up, deserializer.valid_up)
    wire(testcircuit.ready_down, deserializer.ready_down)
    wire(testcircuit.valid_down, deserializer.valid_down)
    wire(testcircuit.CE, deserializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(num_iterations):
        sim.set_value(testcircuit.CE, True, scope)
        sim.set_value(testcircuit.valid_up, True, scope)
        sim.set_value(testcircuit.ready_down, True, scope)

        for j in range(num_in):
            sim.set_value(testcircuit.I, int2seq(i * num_in + j, width), scope)
            sim.evaluate()

            assert sim.get_value(testcircuit.ready_up, scope) == True
            assert sim.get_value(testcircuit.valid_down, scope) == (j == num_in - 1)

            if j == num_in - 1:
                for k in range(num_in):
                    assert seq2int(sim.get_value(testcircuit.O[k], scope)) == i * num_in + k

            sim.evaluate()
            sim.advance_cycle()
            sim.evaluate()

def test_deserializer_rv_and_ce_flicker():
    width = 5
    num_in = 5
    time_per_element = 1
    ce_period = 2
    ready_down_period = 3
    valid_up_period = 4
    num_iterations = 2
    scope = Scope()
    inType = In(Array[width, Bit])
    outType = Out(Array[num_in, Array[width, BitIn]])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('Test_Deserializer', *args)

    deserializer = Deserializer(num_in, time_per_element, inType, has_ce=True)
    wire(deserializer.I, testcircuit.I)
    wire(testcircuit.O, deserializer.O)
    wire(testcircuit.ready_up, deserializer.ready_up)
    wire(testcircuit.valid_up, deserializer.valid_up)
    wire(testcircuit.ready_down, deserializer.ready_down)
    wire(testcircuit.valid_down, deserializer.valid_down)
    wire(testcircuit.CE, deserializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(num_iterations):
        parts_of_elements_accepted = 0
        elements_accepted = 0
        clk = -1
        while elements_accepted < num_in:
            clk += 1
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            # note: parts_of_elements_emitted accounts for parts of all elements, not just current element
            # so this variable covers all parts of all elements for each iteration
            sim.set_value(testcircuit.I, int2seq(i * num_in + parts_of_elements_accepted, width), scope)
            sim.evaluate()
            # check ready
            if clk % ce_period == 0 and (clk % ready_down_period == 0 or elements_accepted != num_in - 1):
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False
            # check valid
            if clk % ce_period == 0 and clk % valid_up_period == 0 and elements_accepted == num_in - 1:
                for k in range(num_in):
                    assert seq2int(sim.get_value(testcircuit.O[k], scope)) == i * num_in + k
                assert sim.get_value(testcircuit.valid_down, scope) == True
            else:
                assert sim.get_value(testcircuit.valid_down, scope) == False
            # increment counters each time circuit executes
            if clk % ce_period == 0 and clk % valid_up_period == 0 and \
                    ((clk % ready_down_period == 0 and elements_accepted == num_in - 1) or elements_accepted != num_in - 1):
                parts_of_elements_accepted += 1
                if parts_of_elements_accepted % time_per_element == 0:
                    elements_accepted += 1
            sim.advance_cycle()
            sim.evaluate()

def test_deserializer_multi_clock_elements():
    width = 5
    num_in = 3
    time_per_element = 2
    num_iterations = 2
    scope = Scope()
    inType = In(Array[width, Bit])
    outType = Out(Array[num_in, Array[width, BitIn]])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('Test_Deserializer', *args)

    deserializer = Deserializer(num_in, time_per_element, inType, has_ce=True)
    wire(deserializer.I, testcircuit.I)
    wire(testcircuit.O, deserializer.O)
    wire(testcircuit.ready_up, deserializer.ready_up)
    wire(testcircuit.valid_up, deserializer.valid_up)
    wire(testcircuit.ready_down, deserializer.ready_down)
    wire(testcircuit.valid_down, deserializer.valid_down)
    wire(testcircuit.CE, deserializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    sim.set_value(testcircuit.CE, True, scope)
    for i in range(num_iterations):
        parts_of_elements_accepted = 0
        elements_accepted = 0
        clk = -1
        while elements_accepted < num_in:
            clk += 1
            # note: parts_of_elements_emitted accounts for parts of all elements, not just current element
            # so this variable covers all parts of all elements for each iteration
            sim.set_value(testcircuit.I, int2seq(i * num_in + parts_of_elements_accepted, width), scope)
            sim.evaluate()

            # check ready
            assert sim.get_value(testcircuit.ready_up, scope) == True

            # check valid
            if elements_accepted == num_in - 1:
                for k in range(num_in):
                    # outputing one part of all elements simultaneously, so need to treat parts_of_elements_accepted differently
                    # need to modulo parts_of_elements_accepted here, unlike above, as checking one part of all elements each clock
                    # and so here parts_of_elements only handling the part of the element.
                    # here, k * time_per_element gets current element.
                    assert seq2int(sim.get_value(testcircuit.O[k], scope)) == i * num_in + \
                           k * time_per_element + (parts_of_elements_accepted % time_per_element)
                assert sim.get_value(testcircuit.valid_down, scope) == True
            else:
                assert sim.get_value(testcircuit.valid_down, scope) == False

            # increment counters each time circuit executes
            parts_of_elements_accepted += 1
            if parts_of_elements_accepted % time_per_element == 0:
                elements_accepted += 1
            sim.advance_cycle()
            sim.evaluate()

def test_deserializer_multi_clock_elements_rv_and_ce_flicker():
    width = 5
    num_in = 3
    time_per_element = 2
    ce_period = 2
    ready_down_period = 3
    valid_up_period = 4
    num_iterations = 2
    scope = Scope()
    inType = In(Array[width, Bit])
    outType = Out(Array[num_in, Array[width, BitIn]])
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(True, False)

    testcircuit = DefineCircuit('Test_Deserializer', *args)

    deserializer = Deserializer(num_in, time_per_element, inType, has_ce=True)
    wire(deserializer.I, testcircuit.I)
    wire(testcircuit.O, deserializer.O)
    wire(testcircuit.ready_up, deserializer.ready_up)
    wire(testcircuit.valid_up, deserializer.valid_up)
    wire(testcircuit.ready_down, deserializer.ready_down)
    wire(testcircuit.valid_down, deserializer.valid_down)
    wire(testcircuit.CE, deserializer.CE)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(num_iterations):
        parts_of_elements_accepted = 0
        elements_accepted = 0
        clk = -1
        while elements_accepted < num_in:
            clk += 1
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            sim.set_value(testcircuit.I, int2seq(i * num_in + parts_of_elements_accepted, width), scope)
            sim.evaluate()
            # check ready
            if clk % ce_period == 0 and (clk % ready_down_period == 0 or elements_accepted != num_in - 1):
                assert sim.get_value(testcircuit.ready_up, scope) == True
            else:
                assert sim.get_value(testcircuit.ready_up, scope) == False
            # check valid
            if clk % ce_period == 0 and clk % valid_up_period == 0 and elements_accepted == num_in - 1:
                for k in range(num_in):
                    assert seq2int(sim.get_value(testcircuit.O[k], scope)) == i * num_in + \
                           k * time_per_element + (parts_of_elements_accepted % time_per_element)
                assert sim.get_value(testcircuit.valid_down, scope) == True
            else:
                assert sim.get_value(testcircuit.valid_down, scope) == False
            # increment counters each time circuit executes
            if clk % ce_period == 0 and clk % valid_up_period == 0 and \
                    ((clk % ready_down_period == 0 and elements_accepted == num_in - 1) or elements_accepted != num_in - 1):
                parts_of_elements_accepted += 1
                if parts_of_elements_accepted % time_per_element == 0:
                    elements_accepted += 1
            sim.advance_cycle()
            sim.evaluate()

