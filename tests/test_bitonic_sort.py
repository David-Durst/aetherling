from aetherling.modules.flip.bitonic_sort import DefineSort2Elements, DefineBitonicMergePow2, \
    DefineBitonicSortPow2, DefineBitonicSort
from magma import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
from aetherling.helpers.fault_helpers import compile_and_run
import fault
import random
from aetherling.modules.term_any_type import TermAnyType
from mantle.coreir import DefineCoreirConst



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
    #tester.print("input0 k: %d, input0 v: %d\n", testcircuit.I0.k, testcircuit.I0.v)
    #tester.print("input1 k: %d, input1 v: %d\n", testcircuit.I1.k, testcircuit.I1.v)
    #tester.print("output0 k: %d, output0 v: %d\n", testcircuit.O0.k, testcircuit.O0.v)
    #tester.print("output1 k: %d, output1 v: %d\n", testcircuit.O1.k, testcircuit.O1.v)
    #tester.print("lt in0: %d, lt in1: %d, lt out k: %d\n", testcircuit.instances[3].I0, testcircuit.instances[3].I1, testcircuit.instances[3].O)
    tester.circuit.O0.k.expect(input1['k'])
    tester.circuit.O0.v.expect(input1['v'])
    tester.circuit.O1.k.expect(input0['k'])
    tester.circuit.O1.v.expect(input0['v'])

    # put the inputs in opposite order to test sorting is constant
    tester.circuit.I1.k = input0['k']
    tester.circuit.I1.v = input0['v']
    tester.circuit.I0.k = input1['k']
    tester.circuit.I0.v = input1['v']
    tester.eval()
    tester.circuit.O0.k.expect(input1['k'])
    tester.circuit.O0.v.expect(input1['v'])
    tester.circuit.O1.k.expect(input0['k'])
    tester.circuit.O1.v.expect(input0['v'])
    compile_and_run(tester)

def test_bitonic_merge_2_elem():
    width = 11

    T = Tuple({"k": Array[width, BitIn], "v": Array[width, BitIn]})

    testcircuit = DefineBitonicMergePow2(T, 2, lambda x: x.k, 0)

    tester = fault.Tester(testcircuit)

    input0 = {"k" : 4, "v" : 2}
    input1 = {"k" : 1, "v" : 1}
    tester.circuit.I[0].k = input0['k']
    tester.circuit.I[0].v = input0['v']
    tester.circuit.I[1].k = input1['k']
    tester.circuit.I[1].v = input1['v']
    tester.eval()
    tester.circuit.O[0].k.expect(input1['k'])
    tester.circuit.O[0].v.expect(input1['v'])
    tester.circuit.O[1].k.expect(input0['k'])
    tester.circuit.O[1].v.expect(input0['v'])
    compile_and_run(tester)

def test_bitonic_merge_4_elem():
    width = 11

    T = Array[width, BitIn]

    testcircuit = DefineBitonicMergePow2(T, 4, lambda x: x, 0)

    tester = fault.Tester(testcircuit)

    inputs = [0, 3, 2, 1]
    tester.circuit.I[0] = inputs[0]
    tester.circuit.I[1] = inputs[1]
    tester.circuit.I[2] = inputs[2]
    tester.circuit.I[3] = inputs[3]
    tester.eval()
    tester.circuit.O[0].expect(inputs[0])
    tester.circuit.O[1].expect(inputs[3])
    tester.circuit.O[2].expect(inputs[2])
    tester.circuit.O[3].expect(inputs[1])
    compile_and_run(tester)

def test_bitonic_merge_8_elem():
    width = 11

    T = Array[width, BitIn]

    testcircuit = DefineBitonicMergePow2(T, 8, lambda x: x, 0)

    tester = fault.Tester(testcircuit)

    inputs = [0, 2, 5, 7, 6, 4, 3, 1]
    tester.circuit.I[0] = inputs[0]
    tester.circuit.I[1] = inputs[1]
    tester.circuit.I[2] = inputs[2]
    tester.circuit.I[3] = inputs[3]
    tester.circuit.I[4] = inputs[4]
    tester.circuit.I[5] = inputs[5]
    tester.circuit.I[6] = inputs[6]
    tester.circuit.I[7] = inputs[7]
    tester.eval()
    tester.circuit.O[0].expect(inputs[0])
    tester.circuit.O[1].expect(inputs[7])
    tester.circuit.O[2].expect(inputs[1])
    tester.circuit.O[3].expect(inputs[6])
    tester.circuit.O[4].expect(inputs[5])
    tester.circuit.O[5].expect(inputs[2])
    tester.circuit.O[6].expect(inputs[4])
    tester.circuit.O[7].expect(inputs[3])
    compile_and_run(tester)

def test_bitonic_merge_8_elem_reverse():
    width = 11

    T = Array[width, BitIn]

    testcircuit = DefineBitonicMergePow2(T, 8, lambda x: x, 1)

    tester = fault.Tester(testcircuit)

    inputs = [0, 2, 5, 7, 6, 4, 3, 1]
    tester.circuit.I[0] = inputs[0]
    tester.circuit.I[1] = inputs[1]
    tester.circuit.I[2] = inputs[2]
    tester.circuit.I[3] = inputs[3]
    tester.circuit.I[4] = inputs[4]
    tester.circuit.I[5] = inputs[5]
    tester.circuit.I[6] = inputs[6]
    tester.circuit.I[7] = inputs[7]
    tester.eval()
    tester.circuit.O[0].expect(inputs[3])
    tester.circuit.O[1].expect(inputs[4])
    tester.circuit.O[2].expect(inputs[2])
    tester.circuit.O[3].expect(inputs[5])
    tester.circuit.O[4].expect(inputs[6])
    tester.circuit.O[5].expect(inputs[1])
    tester.circuit.O[6].expect(inputs[7])
    tester.circuit.O[7].expect(inputs[0])
    compile_and_run(tester)

def test_bitonic_sort_2_elem():
    width = 11

    T = Tuple({"k": Array[width, BitIn], "v": Array[width, BitIn]})

    testcircuit = DefineBitonicSortPow2(T, 2, lambda x: x.k)

    tester = fault.Tester(testcircuit)

    input0 = {"k" : 4, "v" : 2}
    input1 = {"k" : 1, "v" : 1}
    tester.circuit.I[0].k = input0['k']
    tester.circuit.I[0].v = input0['v']
    tester.circuit.I[1].k = input1['k']
    tester.circuit.I[1].v = input1['v']
    tester.eval()
    tester.circuit.O[0].k.expect(input1['k'])
    tester.circuit.O[0].v.expect(input1['v'])
    tester.circuit.O[1].k.expect(input0['k'])
    tester.circuit.O[1].v.expect(input0['v'])
    compile_and_run(tester)

def test_bitonic_sort_4_elem():
    width = 11

    T = Array[width, BitIn]

    testcircuit = DefineBitonicSortPow2(T, 4, lambda x: x)

    tester = fault.Tester(testcircuit)

    inputs = [0, 2, 1, 3]
    tester.circuit.I[0] = inputs[0]
    tester.circuit.I[1] = inputs[1]
    tester.circuit.I[2] = inputs[2]
    tester.circuit.I[3] = inputs[3]
    tester.eval()
    tester.circuit.O[0].expect(inputs[0])
    tester.circuit.O[1].expect(inputs[2])
    tester.circuit.O[2].expect(inputs[1])
    tester.circuit.O[3].expect(inputs[3])
    compile_and_run(tester)

def test_bitonic_sort_8():
    width = 11
    num_el = 8

    T = Array[width, BitIn]

    testcircuit = DefineBitonicSortPow2(T, num_el, lambda x: x)

    tester = fault.Tester(testcircuit)

    inputs = list(range(num_el))
    random.shuffle(inputs)
    outputs = sorted(inputs)
    for i in range(num_el):
        tester.circuit.I[i] = inputs[i]
    tester.eval()
    for i in range(num_el):
        tester.print(str(i) + " output: %d\n", testcircuit.O[i])
    for i in range(num_el):
        tester.circuit.O[i].expect(outputs[i])
    compile_and_run(tester)

def test_bitonic_sort_9():
    width = 11
    num_el = 9

    T = Array[width, BitIn]

    testcircuit = DefineBitonicSort(T, num_el, lambda x: x)

    tester = fault.Tester(testcircuit)

    inputs = list(range(num_el))
    random.shuffle(inputs)
    outputs = sorted(inputs)
    for i in range(num_el):
        tester.circuit.I[i] = inputs[i]
    tester.eval()
    for i in range(num_el):
        tester.print(str(i) + " output: %d\n", testcircuit.O[i])
    for i in range(num_el):
        tester.circuit.O[i].expect(outputs[i])
    compile_and_run(tester)

def test_term():
    width = 11
    T = Array[width, BitIn]

    args = ['I', In(T), 'O', Out(T)]
    testcircuit = DefineCircuit('Test_Term', *args)
    wire(testcircuit.I, testcircuit.O)
    term = TermAnyType(T)
    t_const = DefineCoreirConst(width, 0)()
    wire(t_const.O, term.I)
    EndCircuit()

    tester = fault.Tester(testcircuit)
    tester.circuit.I = 2
    tester.eval()
    tester.circuit.O.expect(2)
    compile_and_run(tester)
