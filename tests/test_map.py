from aetherling.modules.hydrate import Hydrate, Dehydrate
from aetherling.modules.mapFullyParallelSequential import MapParallel
from aetherling.modules.mapPartiallyParallel import MapPartiallyParallel
from magma.backend.coreir_ import CoreIRBackend
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from aetherling.helpers.image_RAM import *
from mantle.coreir.arith import *
from mantle.coreir import DefineCoreirConst
from os.path import dirname, join
import bit_vector

imgSrc = join(dirname(__file__), "custom.png")
imgDst = join(dirname(__file__), "custom_out.png")

#NOTE: since doesn't start with test_, this isn't a test, it's called by other tests
def run_test_map_npxPerClock_mparallelism(pxPerClock, parallelism):
    addAmount = 5
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ClockInterface(False, False) + RAMInterface(imgSrc, True, True, pxPerClock,
                                                       parallelism)

    testcircuit = DefineCircuit('Test_UpsampleDownsample_1PxPerClock', *args)

    imgData = loadImage(imgSrc, pxPerClock)
    pixelType = Array(imgData.bandsPerPixel, Array(imgData.bitsPerBand, Bit))
    bitsToPixelHydrate = MapParallel(cirb, pxPerClock, Hydrate(cirb, pixelType))
    # do an add constant for each band, for each pixel
    addConstants = MapParallel(cirb, pxPerClock,
                               MapParallel(cirb, imgData.bandsPerPixel,
                                           DefineCoreirConst(imgData.bitsPerBand, addAmount)()))
    addOne = MapParallel(cirb, imgData.bandsPerPixel,
                DefineAdd(imgData.bitsPerBand)())

    addParallel = MapPartiallyParallel(cirb, pxPerClock, parallelism,
                                       addOne)
    pixelToBitsDehydrate = MapParallel(cirb, int(pxPerClock*parallelism),
                                       Dehydrate(cirb, pixelType))


    # Note: input image RAM will send data to hydrate,
    # which converts it to form upsample and downsample can use
    # note that these do wiriring to connect the RAMs to edge of test circuit and
    # adjacent node inside circuit
    InputImageRAM(cirb, testcircuit, bitsToPixelHydrate.I, imgSrc, pxPerClock)
    OutputImageRAM(cirb, testcircuit, pixelToBitsDehydrate.out, testcircuit.input_ren,
                   imgSrc, int(parallelism*pxPerClock))
    wire(addParallel.I0, bitsToPixelHydrate.out)
    wire(addParallel.I1, addConstants.out)
    wire(addParallel.O, pixelToBitsDehydrate.I)

    EndCircuit()

    #GetCoreIRModule(cirb, testcircuit).save_to_file("updown_out.json")

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock)
    # run the simulation for all the rows
    for i in range(imgData.numRows):
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()

    def validIfBandIncreasedByAddAmount(imgData, _, resultData):
        for bandIndex in range(imgData.bandsPerPixel*pxPerClock):
            bandStartIndex = bandIndex * imgData.bitsPerBand
            bandEndIndex = (bandIndex + 1) * imgData.bitsPerBand
            if bit_vector.seq2int(imgData.imgAsBits[bandStartIndex:bandEndIndex]) + \
                addAmount != bit_vector.seq2int(resultData):
                return False
        return True

    DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock, validIfBandIncreasedByAddAmount)

def test_map_4pxPerClock_2PpxParallel():
    run_test_map_npxPerClock_mparallelism(4,0.5)
