from aetherling.modules.hydrate import DefineHydrate, DefineDehydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel, DefineMapParallel, DefineNativeMapParallel
from aetherling.modules.map_partially_parallel import MapPartiallyParallel
from aetherling.modules.upsample import DefineUpsampleSequential
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import GetCoreIRModule
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from aetherling.helpers.image_RAM import *
from mantle.coreir.arith import *
from mantle.coreir import DefineCoreirConst
from os.path import dirname, join
import bit_vector
from magma.bitutils import *
import fault
from aetherling.helpers.magma_helpers import ready_valid_interface

imgSrc = join(dirname(__file__), "custom_small.png")
# use this to write the img output image of the test to the folder containing these tests
#imgDst = join(dirname(__file__), "custom_small_out.png")

#NOTE: since doesn't start with test_, this isn't a test, it's called by other tests
def run_test_map_npxPerClock_mparallelism(pxPerClock, parallelism):
    addAmount = 4
    scope = Scope()
    args = ClockInterface(False, False, has_ce=True) + RAMInterface(imgSrc, True, True, pxPerClock,
                                                                    parallelism)

    testcircuit = DefineCircuit('Test_UpsampleDownsample_{}PxPerClock_{}Parallelism'.format(pxPerClock, parallelism), *args)

    imgData = loadImage(imgSrc, pxPerClock)
    pixelType = Array[imgData.bandsPerPixel, Array[imgData.bitsPerBand, Bit]]
    bitsToPixelHydrate = MapParallel(pxPerClock, DefineHydrate(pixelType))
    # do an add constant for each band, for each pixel
    addConstants = DefineNativeMapParallel(pxPerClock,
                               DefineNativeMapParallel(imgData.bandsPerPixel,
                                           DefineCoreirConst(imgData.bitsPerBand, addAmount)))()

    addParallel = MapPartiallyParallel(pxPerClock, parallelism,
                                       DefineMapParallel(imgData.bandsPerPixel,
                                                   DefineAdd(imgData.bitsPerBand)),
                                       has_ce=True)

    pixelToBitsDehydrate = MapParallel(parallelism, DefineDehydrate(pixelType))


    # Note: input image RAM will send data to hydrate,
    # which converts it to form upsample and downsample can use
    # note that these do wiriring to connect the RAMs to edge of test circuit and
    # adjacent node inside circuit
    InputImageRAM(testcircuit, bitsToPixelHydrate.I, imgSrc, pxPerClock, parallelism)
    OutputImageRAM(testcircuit, pixelToBitsDehydrate.out, testcircuit.input_ren,
                   imgSrc, parallelism)
    wire(addParallel.in0, bitsToPixelHydrate.out)
    wire(addParallel.in1, addConstants.O)
    wire(addParallel.out, pixelToBitsDehydrate.I)
    wire(testcircuit.CE, addParallel.CE)

    EndCircuit()

    #mod = GetCoreIRModule(cirb, type(addParallel._instances[1]._instances[1]))
    #mod.save_to_file("test_map_partiallyParallel.json")
    #return

    #cirb.context.run_passes(["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst","flattentypes", "flatten"],
    #                        ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    #mod.save_to_file("test_map_muxnmapped.json")
    #returnx

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    #GetCoreIRModule(cirb, testcircuit).save_to_file("test_map_flattened.json.test")
    #return
    #addParallelScope = Scope(instance=addParallel, parent=scope)
    #addParallelPartitionScope = Scope(instance=addParallel._instances[1], parent=addParallelScope)
    #addParallelPartitionMuxScope = Scope(instance=addParallel._instances[1]._instances[1], parent=addParallelPartitionScope)
    #sim.get_value(addParallel._instances[1]._instances[1].I[0].data, addParallelPartitionScope)
    #dataVal = sim.get_value(addParallel._instances[1]._instances[1].I[0], addParallelPartitionScope)

    #dataVal['sel'] = True
    #sim.set_value(addParallel._instances[1]._instances[1].I[0], dataVal, addParallelPartitionScope)

    LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock)
    # run the simulation for all the rows, adjusting for parallelism as less parallel
    # means more rows
    for i in range(int(imgData.numRows * (pxPerClock / parallelism))):
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()

    def validIfBandIncreasedByAddAmount(imgData, rowIndex, resultData):
        rowBitsStartIndex = rowIndex * imgData.bitsPerRow
        for bandIndex in range(imgData.bandsPerPixel*parallelism):
            bandStartIndex = bandIndex * imgData.bitsPerBand
            bandEndIndex = (bandIndex + 1) * imgData.bitsPerBand
            # need to handle wrap around with mod 256 as ints are 8 bit unsigned ints
            if (bit_vector.seq2int(imgData.imgAsBits[rowBitsStartIndex+bandStartIndex:rowBitsStartIndex+bandEndIndex]) + \
                addAmount) % 256 != bit_vector.seq2int(resultData[bandStartIndex:bandEndIndex]):
                return False
        return True

    DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc, parallelism, validIfBandIncreasedByAddAmount)

def test_map_4pxPerClock_2PxParallel():
    run_test_map_npxPerClock_mparallelism(4,2)

def test_map_merge_rv_ce():
    width = 11
    numIn = 13
    numUp = 5
    T = Array[width, BitIn]

    args = ['I', In(Array[numIn, T]), 'O', Out(Array[numIn, T])] + ClockInterface(True, True) + ready_valid_interface
    testcircuit = DefineCircuit('Test_Map_Merge', *args)

    ups = DefineNativeMapParallel(numIn, DefineUpsampleSequential(numUp, 1, T, True, True), merge_ready_valid_ce_reset=True)()

    wire(ups.I, testcircuit.I)
    wire(ups.O, testcircuit.O)
    wire(ups.CE, testcircuit.CE)
    wire(ups.RESET, testcircuit.RESET)
    wire(ups.valid_down, testcircuit.valid_down)
    wire(ups.valid_up, testcircuit.valid_up)
    wire(ups.ready_down, testcircuit.ready_down)
    wire(ups.ready_up, testcircuit.ready_up)

    EndCircuit()

    magma.compile("vBuild/" + testcircuit.name, testcircuit, verilator_debug=True, output="coreir-verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.ready_down = True
    tester.circuit.valid_up = True
    tester.circuit.CE = True
    tester.circuit.RESET = False
    for i in range(numIn):
        tester.circuit.I[i] = i
    for i in range(numUp):
        tester.eval()
        for j in range(numIn):
            tester.circuit.O[j].expect(j)
        #tester.print(f"circuit ready_up: %d\n", testcircuit.ready_up)
        #tester.print(f"first upsample ready_up: %d\n", testcircuit._instances[0]._instances[0].defn.ready_up)
        #tester.print(f"first upsample ready_down: %d\n", testcircuit._instances[0]._instances[0].defn.ready_down)
        #tester.print(f"first upsample valid_up: %d\n", testcircuit._instances[0]._instances[0].defn.valid_up)
        #tester.print(f"first upsample valid_down: %d\n", testcircuit._instances[0]._instances[0].defn.valid_down)
        tester.circuit.ready_up.expect(i == 0)
        tester.circuit.valid_down.expect(True)
        tester.step(2)
        for i in range(numIn):
            tester.circuit.I[i] = 0
    tester.compile_and_run(target="verilator", magma_opts={"verilator_debug": True}, skip_compile=True, directory="vBuild/")
