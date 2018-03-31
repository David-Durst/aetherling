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

imgSrc = "tests/pillow.jpg"

def test_updown_1pxPerClock():
    upsampleAmount = 7
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ClockInterface(False, False) + RAMInterface(imgSrc, True, True)

    testcircuit = DefineCircuit('Test_UpsampleDownsample_1PxPerClock', *args)

    imgData = loadImage(imgSrc, 1)
    pixelType = Array(imgData.bitsPerPixel, Bit)
    bitsToPixelHydrate = Hydrate(cirb, pixelType)
    upParallel = UpsampleParallel(upsampleAmount, pixelType)
    downParallel = DownsampleParallel(upsampleAmount, pixelType)
    bitsToPixelDehydrate = Dehydrate(cirb, pixelType)

    # Note: input image RAM will send data to hydrate,
    # which converts it to form upsample and downsample can use
    # note that these do wiriring to connect the RAMs to edge of test circuit and
    # adjacent node inside circuit
    InputImageRAM(testcircuit, bitsToPixelHydrate.I, imgSrc)
    OutputImageRAM(testcircuit, bitsToPixelDehydrate.O, imgSrc)
    wire(upParallel.I, bitsToPixelHydrate.O)
    wire(upParallel.O, downParallel.I)
    wire(downParallel.O, bitsToPixelHydrate.I)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, cirb.context)

    LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc)
    DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc)
