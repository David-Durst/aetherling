from aetherling.modules.downsample import DownsampleSequential, DownsampleParallel
from aetherling.modules.hydrate import Hydrate, Dehydrate
from magma.simulator import PythonSimulator
from magma import *
from magma.backend import coreir_compile
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
import bit_vector.bit_vector
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from aetherling.helpers.image_RAM import *
from aetherling.modules.upsample import UpsampleParallel
from aetherling.modules.downsample import DownsampleParallel
from magma.simulator.mdb import simulate
from mantle import CounterModM, Decode
from os.path import dirname, join

imgSrc = join(dirname(__file__), "pillow.jpg")

def test_updown_1pxPerClock():
    upsampleAmount = 7
    pxPerClock = 1
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ClockInterface(False, False) + RAMInterface(imgSrc, True, True, pxPerClock)

    testcircuit = DefineCircuit('Test_UpsampleDownsample_1PxPerClock', *args)

    imgData = loadImage(imgSrc, pxPerClock)
    pixelType = Array(imgData.bitsPerPixel, Bit)
    bitsToPixelHydrate = Hydrate(cirb, pixelType)
    upParallel = UpsampleParallel(upsampleAmount, pixelType)
    downParallel = DownsampleParallel(cirb, upsampleAmount, pixelType)
    bitsToPixelDehydrate = Dehydrate(cirb, pixelType)

    # Note: input image RAM will send data to hydrate,
    # which converts it to form upsample and downsample can use
    # note that these do wiriring to connect the RAMs to edge of test circuit and
    # adjacent node inside circuit
    InputImageRAM(cirb, testcircuit, bitsToPixelHydrate.I, imgSrc, pxPerClock)
    OutputImageRAM(cirb, testcircuit, bitsToPixelDehydrate.out, testcircuit.input_ren, imgSrc, pxPerClock)
    wire(upParallel.I, bitsToPixelHydrate.out)
    wire(upParallel.O, downParallel.I)
    wire(downParallel.O, bitsToPixelDehydrate.I)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context)

    LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock)
    DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock)
