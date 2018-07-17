from aetherling.modules.linebuffer import *
from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from magma.frontend.coreir_ import GetCoreIRModule

def test_linebuffer_1pxPerClock_3pxWindow():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = Array(1, Array(3, BitIn))
    outType = Array(3, Array(3, Out(Bit)))
    imgType = Array(30, Array(3, BitIn))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('lb1_3_Test', *args)

    lb = Linebuffer(cirb, inType, outType, imgType, False)
    wire(lb.I, testcircuit.I)
    wire(testcircuit.O, lb.out)

    wire(1, lb.wen)

    EndCircuit()

def test_linebuffer_2pxPerClock_4pxWindow():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    width = 8
    inType = Array(2, Array(width, BitIn))
    outType = Array(4, Array(width, Out(Bit)))
    imgType = Array(30, Array(width, BitIn))
    args = ['I', inType, 'O', outType, 'valid', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('lb1_3_Test', *args)

    lb = Linebuffer(cirb, inType, outType, imgType, True)
    wire(lb.I, testcircuit.I)
    wire(testcircuit.O, lb.out)

    wire(1, lb.wen)
    wire(lb.valid, testcircuit.valid)

    EndCircuit()

    module = GetCoreIRModule(cirb, testcircuit)
    module.save_to_file("test_linebuffer24.json")
    print("running generators")
    c.run_passes(["rungenerators"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("generators didn't fail")
    print("running wireclocks")
    c.run_passes(["wireclocks-coreir"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("wireclocks didn't fail")
    print("running verifyconnectivity-noclkrst")
    c.run_passes(["verifyconnectivity-noclkrst"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("verifyconnectivity didn't fail")
    c.run_passes(["flattentypes", "flatten"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("flattening didn't fail")
    c.run_passes(["verifyconnectivity-noclkrst", "deletedeadinstances"],
                      ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("verify connectivty and delete dead instances didn't fail")

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(3):
        assert sim.get_value(testcircuit.valid, scope) == (i >= 2)
        if i >= 2:
            assert seq2int(sim.get_value(testcircuit.O[0], scope)) == (i-2)*2
            assert seq2int(sim.get_value(testcircuit.O[0], scope)) == (i-2)*2+1
        sim.set_value(testcircuit.I[0], int2seq(2*i, width), scope)
        sim.set_value(testcircuit.I[1], int2seq(2*i+1, width), scope)
        sim.evaluate()
        sim.advance_cycle()

def test_linebuffer_2pxPerClock_3pxWindow():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    elementType = Array(3, Bit)
    pxPerClock = 2
    stencilWidth = 3
    inType = In(Array(pxPerClock, elementType))
    outType = Out(Array(pxPerClock, Array(stencilWidth, elementType)))
    imgType = Array(30, elementType)
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('lb1_3_Test', *args)

    lb = Linebuffer1DPartitioned(cirb, pxPerClock, stencilWidth, elementType, imgType)
    wire(lb.I, testcircuit.I)
    wire(testcircuit.O, lb.O)

    wire(1, lb.CE)

    EndCircuit()