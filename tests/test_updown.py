from aetherling.modules.hydrate import Hydrate, Dehydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from aetherling.helpers.image_RAM import *
from aetherling.modules.upsample import UpsampleParallel
from aetherling.modules.downsample import DownsampleParallel
from os.path import dirname, join

imgSrc = join(dirname(__file__), "custom_small.png")
# use this to write the img output image of the test to the folder containing these tests
#imgDst = join(dirname(__file__), "custom_small_out.png")

#NOTE: since doesn't start with test_, this isn't a test, it's called by other tests
def run_test_updown_npxPerClock(pxPerClock):
    upsampleAmount = 7
    scope = Scope()
    args = ClockInterface(False, False) + RAMInterface(imgSrc, True, True, pxPerClock)

    testcircuit = DefineCircuit('Test_UpsampleDownsample_{}PxPerClock'.format(pxPerClock), *args)

    imgData = loadImage(imgSrc, pxPerClock)
    pixelType = Array[imgData.bitsPerPixel, Bit]
    bitsToPixelHydrate = MapParallel(pxPerClock, Hydrate(pixelType))
    upParallel = MapParallel(pxPerClock, UpsampleParallel(upsampleAmount, pixelType))
    downParallel = MapParallel(pxPerClock, DownsampleParallel(upsampleAmount, pixelType))
    pixelToBitsDehydrate = MapParallel(pxPerClock, Dehydrate(pixelType))


    # Note: input image RAM will send data to hydrate,
    # which converts it to form upsample and downsample can use
    # note that these do wiriring to connect the RAMs to edge of test circuit and
    # adjacent node inside circuit
    InputImageRAM(testcircuit, bitsToPixelHydrate.I, imgSrc, pxPerClock)
    OutputImageRAM(testcircuit, pixelToBitsDehydrate.out, testcircuit.input_ren, imgSrc, pxPerClock)
    wire(upParallel.I, bitsToPixelHydrate.out)
    wire(upParallel.O, downParallel.I)
    wire(downParallel.O, pixelToBitsDehydrate.I)

    EndCircuit()

    #GetCoreIRModule(testcircuit).save_to_file("updown_out{}.json".format(pxPerClock))

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock)
    # run the simulation for all the rows
    for i in range(imgData.numRows):
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
    DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock, validIfEqual)

def test_updown_1pxPerClock():
    run_test_updown_npxPerClock(1)

def test_updown_5pxPerClock():
    run_test_updown_npxPerClock(5)