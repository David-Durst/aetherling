from aetherling.modules.linebuffer import *
from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle import CounterModM, Decode

def test_linebuffer_1pxPerClock_3pxWindow():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = Array(1, Array(3, BitIn))
    outType = Array(3, Array(3, Out(Bit)))
    imgType = Array(30, Array(3, BitIn))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('lb1_3_Test', *args)

    lb = Linebuffer(cirb, inType, outType, imgType, False)
    wire(lb.I, testcircuit.I)
    wire(testcircuit.O, lb.out)

    wire(1, lb.wen)

    EndCircuit()

def test_linebuffer_2pxPerClock_4pxWindow():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = Array(2, Array(3, BitIn))
    outType = Array(4, Array(3, Out(Bit)))
    imgType = Array(30, Array(3, BitIn))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('lb1_3_Test', *args)

    lb = Linebuffer(cirb, inType, outType, imgType, False)
    wire(lb.I, testcircuit.I)
    wire(testcircuit.O, lb.out)

    wire(1, lb.wen)

    EndCircuit()

def test_linebuffer_2pxPerClock_3pxWindow():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    elementType = Array(3, Bit)
    pxPerClock = 2
    stencilWidth = 3
    inType = In(Array(pxPerClock, elementType))
    outType = Out(Array(pxPerClock, Array(stencilWidth, elementType)))
    imgType = Array(30, elementType)
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('lb1_3_Test', *args)

    lb = Linebuffer1DPartitioned(cirb, pxPerClock, stencilWidth, elementType, imgType)
    wire(lb.I, testcircuit.I)
    wire(testcircuit.O, lb.O)

    wire(1, lb.CE)

    EndCircuit()