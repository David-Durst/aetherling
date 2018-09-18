from aetherling.modules.hydrate import DefineHydrate, DefineDehydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel, DefineMapParallel, DefineNativeMapParallel
from aetherling.modules.map_partially_parallel import MapPartiallyParallel
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

imgSrc = join(dirname(__file__), "custom_small.png")
# use this to write the img output image of the test to the folder containing these tests
#imgDst = join(dirname(__file__), "custom_small_out.png")

#NOTE: since doesn't start with test_, this isn't a test, it's called by other tests
def run_test_map_npxPerClock_mparallelism(pxPerClock, parallelism):
    addAmount = 4
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ClockInterface(False, False, has_ce=True) + RAMInterface(imgSrc, True, True, pxPerClock,
                                                                    parallelism)

    testcircuit = DefineCircuit('Test_UpsampleDownsample_{}PxPerClock_{}Parallelism'.format(pxPerClock, parallelism), *args)

    imgData = loadImage(imgSrc, pxPerClock)
    pixelType = Array(imgData.bandsPerPixel, Array(imgData.bitsPerBand, Bit))
    bitsToPixelHydrate = MapParallel(cirb, pxPerClock, DefineHydrate(cirb, pixelType))
    # do an add constant for each band, for each pixel
    addConstants = DefineNativeMapParallel(cirb, pxPerClock,
                               DefineNativeMapParallel(cirb, imgData.bandsPerPixel,
                                           DefineCoreirConst(imgData.bitsPerBand, addAmount)))()

    addParallel = MapPartiallyParallel(cirb, pxPerClock, parallelism,
                                       DefineMapParallel(cirb, imgData.bandsPerPixel,
                                                   DefineAdd(imgData.bitsPerBand)),
                                       has_ce=True)

    pixelToBitsDehydrate = MapParallel(cirb, parallelism, DefineDehydrate(cirb, pixelType))


    # Note: input image RAM will send data to hydrate,
    # which converts it to form upsample and downsample can use
    # note that these do wiriring to connect the RAMs to edge of test circuit and
    # adjacent node inside circuit
    InputImageRAM(cirb, testcircuit, bitsToPixelHydrate.I, imgSrc, pxPerClock, parallelism)
    OutputImageRAM(cirb, testcircuit, pixelToBitsDehydrate.out, testcircuit.input_ren,
                   imgSrc, parallelism)
    wire(addParallel.in0, bitsToPixelHydrate.out)
    wire(addParallel.in1, addConstants.O)
    wire(addParallel.out, pixelToBitsDehydrate.I)
    wire(testcircuit.CE, addParallel.CE)

    EndCircuit()

    #mod = GetCoreIRModule(cirb, type(addParallel._instances[1]._instances[1]))
    #mod.save_to_file("test_map_partiallyParallel.json")
    #return

    #cirb.context.run_passes(["rungenerators", "wireclocks-coreir", "verifyconnectivity-noclkrst","flattentypes", "flatten"],
    #                        ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    #mod.save_to_file("test_map_muxnmapped.json")
    #returnx

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
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
