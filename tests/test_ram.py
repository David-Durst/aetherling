from aetherling.modules.ram_any_type import DefineRAMAnyType
from magma import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run
import builtins

def test_ram_11_13():
    width = 11
    numOut = 13
    T = Array[width, BitOut]

    testcircuit = DefineRAMAnyType(T, numOut)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.WE = True
    for i in range(numOut + 1):
        tester.circuit.RADDR = i % numOut
        tester.circuit.WADDR = i % numOut
        tester.circuit.WDATA = i % numOut
        tester.step(2)
        tester.circuit.RDATA.expect(i % numOut)
    compile_and_run(tester)

def test_ram_1_13():
    width = 11
    numOut = 1
    T = Array[width, BitOut]

    testcircuit = DefineRAMAnyType(T, numOut)

    tester = fault.Tester(testcircuit, testcircuit.CLK)
    tester.circuit.WE = True
    tester.circuit.RADDR = 0
    tester.circuit.WADDR = 0
    tester.circuit.WDATA = 2
    tester.step(2)
    tester.circuit.RDATA.expect(2)
    compile_and_run(tester)
