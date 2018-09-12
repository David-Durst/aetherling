from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend, compile
from aetherling.modules.delayed_buffer import DefineDelayedBuffer
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope

def test_delayed_buffer():
    width = 5
    numOut = 2
    testVal = 3
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = Array(width, In(BitIn))
    outType = Out(inType)

    testcircuit = DefineDelayedBuffer(cirb, Array(4, Bit), 6, 2, 12)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context)

if __name__ == "__main__":
    test_delayed_buffer()

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