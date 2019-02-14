from aetherling.modules.noop import DefineNoop
from magma import *
from mantle.coreir import DefineCoreirConst
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend, compile
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.common.countermod import CounterModM, Decode
from aetherling.modules.native_linebuffer.two_dimensional_native_linebuffer import DefineTwoDimensionalLineBuffer
from aetherling.modules.native_linebuffer.one_dimensional_native_linebuffer import DefineOneDimensionalLineBuffer
from aetherling.modules.delayed_buffer import DefineDelayedBuffer
from mantle.coreir.memory import DefineRAM, getRAMAddrWidth
from aetherling.modules.ram_any_type import DefineRAMAnyType
from magma.frontend.coreir_ import GetCoreIRModule
import fault

def test_2dlb_flicker_ce_with_2x2_stride():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 2, 2, 0, 0, True)

    #magma.compile("2dlbflicker_1001pm_2_11_19_unflattened", testcircuit, output="verilog",
    #              passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "verifyconnectivity --noclkrst", "deletedeadinstances"],
    #              namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)
    #magma.compile("2dlbflicker_912pm_2_11_19_flattened", testcircuit, output="verilog",
    #              passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
    #              namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)
    #tcm = GetCoreIRModule(cirb, testcircuit)
    #cirb.context.run_passes(, )
    #tcm.save_to_file("2dlbflicker.json")


    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(100000):
        if i % 2 == 0:
            sim.set_value(testcircuit.I[0][0], int2seq(1, 8), scope)
            sim.set_value(testcircuit.CE, 1, scope)
        else:
            sim.set_value(testcircuit.I[0][0], int2seq(2, 8), scope)
            sim.set_value(testcircuit.CE, 0, scope)
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
        print("i" + str(i))
        print("undelayed out: " + str([seq2int(sim.get_value(testcircuit.undelayedO[0][r][c], scope)) for r in range(2) for c in range(2)]))
        print("out: " + str([seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) for r in range(2) for c in range(2)]))
        print("valid: " + str(sim.get_value(testcircuit.valid, scope)))
        print("db CE: " + str(sim.get_value(testcircuit.dbCE, scope)))
        print("db WE: " + str(sim.get_value(testcircuit.dbWE, scope)))
        print("db RDATA: " + str([seq2int(sim.get_value(testcircuit.RDATA, scope)[0][r][c]) for r in range(2) for c in range(2)]))
        print("db WDATA: " + str([seq2int(sim.get_value(testcircuit.WDATA, scope)[0][r][c]) for r in range(2) for c in range(2)]))
        print("db RADDR: " + str(seq2int(sim.get_value(testcircuit.RADDR, scope)[0])))
        print("db WADDR: " + str(seq2int(sim.get_value(testcircuit.WADDR, scope)[0])))
        print("db RAM WE: " + str(sim.get_value(testcircuit.RAMWE, scope)))
        print("")
        print("")

        # for some reason, lb going to 0 when flickering valid on and off for ce
        #for r in range(2):
        #    for c in range(2):
        #        assert (sim.get_value(testcircuit.valid, scope) == 0 or seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) != 0)

def test_1dlb_flicker_ce_with_2_stride():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineOneDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 2, 8, 2, 0, True)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(50):
        if i % 2 == 0:
            sim.set_value(testcircuit.I[0], int2seq(1, 8), scope)
            sim.set_value(testcircuit.CE, 1, scope)
        else:
            sim.set_value(testcircuit.I[0], int2seq(2, 8), scope)
            sim.set_value(testcircuit.CE, 0, scope)
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
        print([seq2int(sim.get_value(testcircuit.O[0][r], scope)) for r in range(2)])
        print(sim.get_value(testcircuit.valid, scope))
        # for some reason, lb going to 0 when flickering valid on and off for ce
        for r in range(2):
            assert (sim.get_value(testcircuit.valid, scope) == 0 or seq2int(sim.get_value(testcircuit.O[0][r], scope)) != 0)

def test_2dlb_flicker_ce_with_no_stride():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 1, 1, 0, 0, True)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(100000):
        if i % 2 == 0:
            sim.set_value(testcircuit.I[0][0], int2seq(1, 8), scope)
            sim.set_value(testcircuit.CE, 1, scope)
        else:
            sim.set_value(testcircuit.I[0][0], int2seq(2, 8), scope)
            sim.set_value(testcircuit.CE, 0, scope)
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
        print([seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) for r in range(2) for c in range(2)])
        print(sim.get_value(testcircuit.valid, scope))
        # for some reason, lb going to 0 when flickering valid on and off for ce
        for r in range(2):
            for c in range(2):
                assert (sim.get_value(testcircuit.valid, scope) == 0 or seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) != 0)

def test_db_flicker_ce():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineDelayedBuffer(cirb, Array(8, Bit), 4, 1, 16, 1)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(100000):
        if i % 2 == 0 and i > 20:
            sim.set_value(testcircuit.I[0], int2seq(1, 8), scope)
            sim.set_value(testcircuit.CE, 1, scope)
        else:
            sim.set_value(testcircuit.I[0], int2seq(2, 8), scope)
            sim.set_value(testcircuit.CE, 0, scope)
        if i % 2 == 0 and i > 20: #(not i % 16 == 0 or i == 0):
            sim.set_value(testcircuit.WE, 1, scope)
        else:
            sim.set_value(testcircuit.WE, 0, scope)
        sim.evaluate()
        print(seq2int(sim.get_value(testcircuit.O[0], scope)))
        print(sim.get_value(testcircuit.valid, scope))

        # for some reason, lb going to 0 when flickering valid on and off for ce
        assert (sim.get_value(testcircuit.valid, scope) == 0 or seq2int(sim.get_value(testcircuit.O[0], scope)) != 0)
        sim.advance_cycle()

def test_ram_flicker_we():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineRAM(8, 1)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(testcircuit.WADDR[0], 0, scope)
    sim.set_value(testcircuit.RADDR[0], 0, scope)
    sim.set_value(testcircuit.WDATA[0], 1, scope)
    for i in range(20):
        sim.set_value(testcircuit.WE, 0, scope)
        sim.evaluate()
        sim.advance_cycle()
    sim.set_value(testcircuit.WE, 1, scope)
    sim.evaluate()
    sim.advance_cycle()
    sim.set_value(testcircuit.WE, 0, scope)
    sim.evaluate()
    sim.advance_cycle()
    print(sim.get_value(testcircuit.RDATA, scope))

def test_any_type_ram_flicker_we():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineRAMAnyType(cirb, Array(4, Array(8, Bit)), 4) #DefineRAM(8, 1)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(testcircuit.WADDR[0], 0, scope)
    sim.set_value(testcircuit.RADDR[0], 0, scope)
    sim.set_value(testcircuit.WDATA[0], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WDATA[1], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WDATA[2], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WDATA[3], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WE, 0, scope)
    for i in range(20):
        sim.evaluate()
        if i == 0:
            sim.set_value(testcircuit.WDATA[0], int2seq(1, 8), scope)
        if i == 2:
            sim.set_value(testcircuit.WDATA[1], int2seq(1, 8), scope)
        if i == 16:
            sim.set_value(testcircuit.WDATA[2], int2seq(1, 8), scope)
        if i == 18:
            sim.set_value(testcircuit.WDATA[3], int2seq(1, 8), scope)
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
        print("WDATA: " + str([seq2int(sim.get_value(testcircuit.WDATA, scope)[r]) for r in range(4)]))
        print("RDATA: " + str([seq2int(sim.get_value(testcircuit.RDATA, scope)[r]) for r in range(4)]))
    sim.set_value(testcircuit.WE, 1, scope)
    sim.evaluate()
    sim.advance_cycle()
    sim.evaluate()
    sim.set_value(testcircuit.WDATA[0], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WDATA[1], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WDATA[2], int2seq(0, 8), scope)
    sim.set_value(testcircuit.WDATA[3], int2seq(0, 8), scope)
    print("RDATA: " + str([seq2int(sim.get_value(testcircuit.RDATA, scope)[r]) for r in range(4)]))
    sim.set_value(testcircuit.WE, 0, scope)
    sim.evaluate()
    sim.advance_cycle()
    sim.evaluate()
    print("RDATA: " + str([seq2int(sim.get_value(testcircuit.RDATA, scope)[r]) for r in range(4)]))

def test_clock_adjusted_2dlb_flicker_ce_with_2x2_stride():
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 2, 2, 0, 0, True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    for i in range(30):
        if i % 2 == 0:
            tester.circuit.I[0][0] = int2seq(1, 8)
            tester.circuit.CE = 1
        else:
            tester.circuit.I[0][0] = int2seq(2, 8)
            tester.circuit.CE = 0

        tester.eval()

        # for some reason, lb going to 0 when flickering valid on and off for ce
        for r in range(2):
            for c in range(2):
                if i >= 21:
                    tester.circuit.O[0][r][c].expect(1)
    tester.compile_and_run("coreir")


def test_2dlb_flicker_ce_with_2x2_stride_ross_clock_instructions():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 2, 2, 0, 0, True)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=c,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(testcircuit.CE, 1, scope)
    sim.set_value(testcircuit.I[0][0], int2seq(1, 8), scope)
    sim.evaluate()
    sim.set_value(testcircuit.CLK, 1, scope)
    sim.evaluate()
    for i in range(100000):
        sim.set_value(testcircuit.CLK, 0, scope)
        sim.evaluate()
        print("CLK: " + str(sim.get_value(testcircuit.CLK, scope)))
        print("i" + str(i))
        print("undelayed out: " + str([seq2int(sim.get_value(testcircuit.undelayedO[0][r][c], scope)) for r in range(2) for c in range(2)]))
        print("out: " + str([seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) for r in range(2) for c in range(2)]))
        print("valid: " + str(sim.get_value(testcircuit.valid, scope)))
        print("db CE: " + str(sim.get_value(testcircuit.dbCE, scope)))
        print("db WE: " + str(sim.get_value(testcircuit.dbWE, scope)))
        print("db RDATA: " + str([seq2int(sim.get_value(testcircuit.RDATA, scope)[0][r][c]) for r in range(2) for c in range(2)]))
        print("db WDATA: " + str([seq2int(sim.get_value(testcircuit.WDATA, scope)[0][r][c]) for r in range(2) for c in range(2)]))
        print("db RADDR: " + str(seq2int(sim.get_value(testcircuit.RADDR, scope)[0])))
        print("db WADDR: " + str(seq2int(sim.get_value(testcircuit.WADDR, scope)[0])))
        print("db RAM WE: " + str(sim.get_value(testcircuit.RAMWE, scope)))
        print("")
        print("")

        sim.set_value(testcircuit.CLK, 1, scope)
        sim.evaluate()
        print("CLK: " + str(sim.get_value(testcircuit.CLK, scope)))

        if i % 2 == 1:
            sim.set_value(testcircuit.I[0][0], int2seq(1, 8), scope)
            sim.set_value(testcircuit.CE, 1, scope)
        else:
            sim.set_value(testcircuit.I[0][0], int2seq(2, 8), scope)
            sim.set_value(testcircuit.CE, 0, scope)

        sim.evaluate()
