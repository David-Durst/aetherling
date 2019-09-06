from aetherling.modules.rom_any_type import DefineROMAnyType
from magma import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run
import builtins

def test_rom_11_13():
    width = 11
    numOut = 13
    T = Array[width, BitOut]
    init = [builtins.tuple(int2seq(i)) for i in range(4, numOut+4)]

    return
    testcircuit = DefineROMAnyType(T, width, builtins.tuple(init))

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.RE = True

    for i in range(numOut):
        tester.circuit.RADDR = i
        tester.step(2)
        tester.circuit.RDATA.expect(i + 4)
    compile_and_run(tester)

