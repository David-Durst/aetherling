from aetherling.modules.flip.bitonic_sort import DefineSort2Elements, DefineBitonicMergePow2, DefineBitonicSortPow2
from magma import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
from aetherling.helpers.fault_helpers import compile_and_run
import fault

def test_sort2_elements():
    width = 11
    T = Tuple({"k": Array[width, BitIn], "v": Array[width, BitIn]})

    testcircuit = DefineSort2Elements(T, lambda x: x.k)

    tester = fault.Tester(testcircuit)

    input0 = {"k" : 4, "v" : 2}
    input1 = {"k" : 1, "v" : 1}
    tester.circuit.I0.k = input0['k']
    tester.circuit.I0.v = input0['v']
    tester.circuit.I1.k = input1['k']
    tester.circuit.I1.v = input1['v']
    tester.eval()
    tester.print("input0 k: %d, input0 v: %d\n", testcircuit.I0.k, testcircuit.I0.v)
    tester.print("input1 k: %d, input1 v: %d\n", testcircuit.I1.k, testcircuit.I1.v)
    tester.print("output0 k: %d, output0 v: %d\n", testcircuit.O0.k, testcircuit.O0.v)
    tester.print("output1 k: %d, output1 v: %d\n", testcircuit.O1.k, testcircuit.O1.v)
    #tester.print("lt in0: %d, lt in1: %d, lt out k: %d\n", testcircuit.instances[3].I0, testcircuit.instances[3].I1, testcircuit.instances[3].O)
    tester.circuit.O0.k.expect(input1['k'])
    tester.circuit.O0.v.expect(input1['v'])
    tester.circuit.O1.k.expect(input0['k'])
    tester.circuit.O1.v.expect(input0['v'])
    compile_and_run(tester)

