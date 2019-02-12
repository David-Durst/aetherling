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
import fault

def test_2dlb_flicker_ce_with_2x2_stride():
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 2, 2, 0, 0, True)


    passes = ["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"]
    namespaces = ["aetherlinglib", "commonlib", "mantle", "coreir", "global"]
    #magma.compile(f"build/{testcircuit.name}", testcircuit, output="coreir-verilog",
    #              passes=passes, namespaces=namespaces, context=c)

    import fault
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    for i in range(100):
        if i % 2 == 0:
            tester.poke(testcircuit.I[0][0], 1) #int2seq(1, 8))
            tester.poke(testcircuit.CE, 1)
        else:
            tester.poke(testcircuit.I[0][0], 2) #int2seq(2, 8))
            tester.poke(testcircuit.CE, 0)
        tester.eval()
        tester.step(2)
        tester.eval()
        # print("i" + str(i))
        # print("undelayed out: " + str([seq2int(sim.get_value(testcircuit.undelayedO[0][r][c], scope)) for r in range(2) for c in range(2)]))
        # print("out: " + str([seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) for r in range(2) for c in range(2)]))
        # print("valid: " + str(sim.get_value(testcircuit.valid, scope)))
        # print("db CE: " + str(sim.get_value(testcircuit.dbCE, scope)))
        # print("db WE: " + str(sim.get_value(testcircuit.dbWE, scope)))
        tester.print(testcircuit.RDATA, f"i={i}  RDATA:  %x")
        tester.print(testcircuit.WDATA, f"i={i}  WDATA:  %x")
        tester.print(testcircuit.RADDR, f"i={i}  RADDR:  %x")
        tester.print(testcircuit.WADDR, f"i={i}  WADDR:  %x")
        tester.print(testcircuit.RAMWE, f"i={i}  RAM WE: %x")
        tester.print(tester.circuit.DelayedBuffer_Array_2_Array_2_Array_8_In_Bit____t_4n_1k_16emittingPeriod_0initialDelay_inst0.MapParallel_n1_opRAM_Array_2_Array_2_Array_8_In_Bit____t_4n_RADDR_In_Bits_2___RDATA_Array_2_Array_2_Array_8_Out_Bit_____WADDR_In_Bits_2___WDATA_Array_2_Array_2_Array_8_In_Bit_____WE_In_Bit__CLK_In_Clock___inst0.RDATA.port, f"derp i={i} RDATA: %x")
        tester.print(tester.circuit.DelayedBuffer_Array_2_Array_2_Array_8_In_Bit____t_4n_1k_16emittingPeriod_0initialDelay_inst0.MapParallel_n1_opRAM_Array_2_Array_2_Array_8_In_Bit____t_4n_RADDR_In_Bits_2___RDATA_Array_2_Array_2_Array_8_Out_Bit_____WADDR_In_Bits_2___WDATA_Array_2_Array_2_Array_8_In_Bit_____WE_In_Bit__CLK_In_Clock___inst0.WDATA.port, f"derp i={i} WDATA: %x")
        tester.print(tester.circuit.DelayedBuffer_Array_2_Array_2_Array_8_In_Bit____t_4n_1k_16emittingPeriod_0initialDelay_inst0.MapParallel_n1_opRAM_Array_2_Array_2_Array_8_In_Bit____t_4n_RADDR_In_Bits_2___RDATA_Array_2_Array_2_Array_8_Out_Bit_____WADDR_In_Bits_2___WDATA_Array_2_Array_2_Array_8_In_Bit_____WE_In_Bit__CLK_In_Clock___inst0.RADDR.port, f"derp i={i} RADDR: %x")
        tester.print(tester.circuit.DelayedBuffer_Array_2_Array_2_Array_8_In_Bit____t_4n_1k_16emittingPeriod_0initialDelay_inst0.MapParallel_n1_opRAM_Array_2_Array_2_Array_8_In_Bit____t_4n_RADDR_In_Bits_2___RDATA_Array_2_Array_2_Array_8_Out_Bit_____WADDR_In_Bits_2___WDATA_Array_2_Array_2_Array_8_In_Bit_____WE_In_Bit__CLK_In_Clock___inst0.WADDR.port, f"derp i={i} WADDR: %x")
        # tester.print(tester.circuit.DelayedBuffer_Array_2_Array_2_Array_8_In_Bit____t_4n_1k_16emittingPeriod_0initialDelay_inst0.MapParallel_n1_opRAM_Array_2_Array_2_Array_8_In_Bit____t_4n_RADDR_In_Bits_2___RDATA_Array_2_Array_2_Array_8_Out_Bit_____WADDR_In_Bits_2___WDATA_Array_2_Array_2_Array_8_In_Bit_____WE_In_Bit__CLK_In_Clock___inst0.WE.port, f"derp i={i} WE: %x")
        # print("")
        # print("")

        # # for some reason, lb going to 0 when flickering valid on and off for ce
        # for r in range(2):
        #     for c in range(2):
        #         assert (sim.get_value(testcircuit.valid, scope) == 0 or seq2int(sim.get_value(testcircuit.O[0][r][c], scope)) != 0)
        if i > 20:
            tester.print(testcircuit.O[0][0][0])

    tester.compile_and_run(target="verilator", skip_compile=True, directory="tests/build/")
    return


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
    scope = Scope()
    c = coreir.Context()
    cirb = CoreIRBackend(c)

    testcircuit = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 2, 2, 0, 0, True)

    tester = fault.tester(testcircuit, testcircuit.CLK)

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

