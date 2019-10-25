from aetherling.modules.register_any_type import DefineRegisterAnyType
from magma import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run
import builtins

def test_reg_11():
    width = 11
    numOut = 1
    T = Array[width, BitOut]

    testcircuit = DefineRegisterAnyType(T, numOut, has_ce=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)
    tester.circuit.CE = 1
    tester.circuit.I = 2
    tester.step(2)
    tester.circuit.O.expect(2)
    compile_and_run(tester)

def test_reg_tuple_11_and_12():
    width0 = 11
    width1 = 12
    numOut = 1
    T = Tuple(Array[width0, BitOut], Array[width1, BitOut])

    testcircuit = DefineRegisterAnyType(T, numOut, has_ce=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)
    tester.circuit.CE = 1
    tester.circuit.I[0] = 2
    tester.circuit.I[1] = 3
    tester.step(2)
    tester.circuit.O[0].expect(2)
    tester.circuit.O[1].expect(3)
    compile_and_run(tester)

def test_reg_array_of_array():
    width = 11
    num = 2
    numOut = 1
    T = Array[num, Array[width, BitOut]]

    testcircuit = DefineRegisterAnyType(T, numOut, has_ce=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)
    tester.circuit.CE = 1
    tester.circuit.I[0] = 2
    tester.circuit.I[1] = 3
    tester.step(2)
    tester.circuit.O[0].expect(2)
    tester.circuit.O[1].expect(3)
    compile_and_run(tester)
