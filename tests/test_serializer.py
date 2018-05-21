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

def test_serializer():
    width = 11
    numIn = 13
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = In(Array(numIn, Array(width, BitIn)))
    outType = Out(Array(width, Bit))
    args = ['I', inType, 'O', outType, 'ready', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Serializer', *args)

    serializer = Serializer(cirb, inType.T, numIn)
    wire(serializer.I, testcircuit.I)
    wire(testcircuit.O, serializer.out)
    wire(testcircuit.ready, serializer.ready)

    EndCircuit()

    module = GetCoreIRModule(cirb, testcircuit)
    module.save_to_file("test_serializer_nopasses.json")
    print("running generators")
    c.run_passes(["rungenerators"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("generators didn't fail")
    print("running wireclocks")
    c.run_passes(["wireclocks-coreir"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("wireclocks didn't fail")
    module.save_to_file("test_serializer_postrungenerators_postwireclocks.json")
    print("running verifyconnectivity-noclkrst")
    c.run_passes(["verifyconnectivity-noclkrst"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("verifyconnectivity didn't fail")
    c.run_passes(["flattentypes", "flatten"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("flattening didn't fail")
    c.run_passes(["verifyconnectivity-noclkrst", "deletedeadinstances"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    print("verify connectivty and delete dead instances didn't fail")

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I[i], int2seq(i, width), scope)

    for i in range(numIn):
        assert sim.get_value(testcircuit.ready, scope) == (i == 0)
        assert seq2int(sim.get_value(testcircuit.O, scope)) == i
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
    assert sim.get_value(testcircuit.ready, scope) == True

def test_deserializer():
    width = 11
    numIn = 13
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = In(Array(width, Bit))
    outType = Out(Array(numIn, Array(width, BitIn)))
    args = ['I', inType, 'O', outType, 'valid', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Deserializer', *args)

    deserializer = Deserializer(cirb, inType.T, numIn)
    wire(deserializer.I, testcircuit.I)
    wire(testcircuit.O, deserializer.out)
    wire(testcircuit.valid, deserializer.valid)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I, int2seq(i, width), scope)
        assert sim.get_value(testcircuit.valid, scope) == (i == numIn - 1)
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
    assert sim.get_value(testcircuit.valid, scope) == False
