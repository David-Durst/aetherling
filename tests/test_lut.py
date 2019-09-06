from aetherling.modules.lut_any_type import DefineLUTAnyType
from magma import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run
import builtins

def test_lut_11_13():
    width = 11
    numOut = 13
    T = Array[width, BitOut]
    init = [builtins.tuple(int2seq(i, width)) for i in range(4, numOut+4)]

    testcircuit = DefineLUTAnyType(T, numOut, builtins.tuple(init))

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    for i in range(numOut):
        tester.circuit.addr = i
        tester.eval()
        tester.circuit.data.expect(i + 4)
    compile_and_run(tester)

