from aetherling.modules.downsample import DownsampleSequential, DownsampleParallel
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
from magma.simulator.mdb import simulate
from mantle import CounterModM, Decode

imgSrc = "tests/pillow.jpg"

def test_updown_1pxPerClock():
    bitsPerPixel = 8
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ClockInterface(False, False) + RAMInterface(imgSrc, True, True)
