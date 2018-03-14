from aetherling.modules.up import UpSequential, UpParallel
from magma.simulator import PythonSimulator
from magma import *
from magma.backend import coreir_compile
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
import bit_vector.bit_vector
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from magma.simulator.mdb import simulate

def test_binary_primitive():

    binary_primitive = ("and", lambda x,y: x & y)
    width = 4;
    input0 = range(16)
    input1 = range(16)
    context = Context()
    primitive_name, primitive_op = binary_primitive
    coreir_primitive = context.import_generator("coreir", primitive_name)
    primitive16 = coreir_primitive(width=width)
    module_typ = context.Record({
        "in0": context.Array(width, context.BitIn()),
        "in1": context.Array(width, context.BitIn()),
        "out": context.Array(width, context.Bit())
    })
    module = context.global_namespace.new_module(f"test_{primitive_name}", module_typ)
    module_def = module.new_definition()
    primitive16_inst = module_def.add_module_instance(f"{primitive_name}_inst", primitive16)
    interface = module_def.interface
    module_def.connect(interface.select("in0"), primitive16_inst.select("in0"))
    module_def.connect(interface.select("in1"), primitive16_inst.select("in1"))
    module_def.connect(interface.select("out"), primitive16_inst.select("out"))
    module.definition = module_def
    sim_primitive16 = coreir.SimulatorState(module)

    a = bit_vector.BitVector(16, 16)
    b = bit_vector.BitVector(16, 16)
    sim_primitive16.set_value(["self.in0"], a.as_bool_list())
    sim_primitive16.set_value(["self.in1"], b.as_bool_list())
    sim_primitive16.execute()
    #assert BitVector(sim_primitive16.get_value(["self"], ["out"])) == primitive_op(a, b)


def test_up_parallel():
    width = 5
    numElements = 1
    testVal = 3
    c = coreir.Context()
    scope = Scope()
    inType = Array(width, In(BitIn))
    outType = Array(numElements, Out(inType))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    upParallel = UpParallel(numElements, inType)
    wire(upParallel.I, testcircuit.I)
    wire(testcircuit.O, upParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    sim.evaluate()
    sim.advance()
    sim.evaluate()
    for i in range(numElements):
        assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testVal

def test_up_sequential():
    width = 5
    numElements = 1
    testVal = 3
    c = coreir.Context()
    scope = Scope()
    inType = Array(width, In(BitIn))
    outType = Array(numElements, Out(inType))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    upSequential = UpSequential(CoreIRBackend(c), numElements, inType)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    sim.evaluate()
    sim.advance()
    sim.evaluate()
    for i in range(numElements):
        assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testVal


if __name__ == "__main__":
    test_up_parallel()

"""

width = 5
numElements = 1
testVal = 3
c = coreir.Context()
scope = Scope()
inType = Array(width, In(BitIn))
#outType = Array(width, Out(BitIn)) #uncomment this line
#outType = Array(numElements, Out(inType))
outType = Array(numElements, Out(Array(width, Bit))) #comment this line
args = ['I', inType, 'O', outType] + ClockInterface(False, False)

testcircuit = DefineCircuit('Test', *args)

#upParallel = UpParallel(numElements, inType)
#wire(upParallel.I, testcircuit.I)
#wire(testcircuit.O, upParallel.O)
#wire(testcircuit.O, testcircuit.I) # uncomment this line
wire(testcircuit.O[0], testcircuit.I) # comment this line


EndCircuit()

sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

#coreir_testcircuit = coreir_compile(testcircuit, context=c)

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