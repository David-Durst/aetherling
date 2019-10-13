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
