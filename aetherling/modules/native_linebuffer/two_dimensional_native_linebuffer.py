from aetherling.modules.hydrate import Dehydrate, Hydrate
from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from mantle.common.sipo import SIPO
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from mantle.coreir.type_helpers import Term
from math import ceil


def DefineTwoDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        stride: int,
        origin: int,
        first_row: bool = True,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
    :param pixel_per_clock_per_row: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the first pixel of the first window relative to
    the top left corner of the image
    :param first_row: True if this is the first 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should not set this unless they understand the internals of the LineBuffer.
    Its used for determining when to reverse inputs to convert image coordinate to internal coordinates.
    :param last_row: True if this is the last 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should probably leave this to true. Its used for adding extra ports when putting these in
    larger matrices.

    Restrictions:
    1. image_size % pixel_per_clock == 0
    2. image_size % stride == 0
    3. stride % pixel_per_clock == 0 OR pixel_per_clock % stride == 0
    4. window_width > |origin|
    5. origin <= 0
    6. window_width - origin < image_size

    :return: A 1D Linebuffer
    """
    pass

def OneDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        output_stride: int,
        origin: int,
        first_row: bool = True,
        last_row: bool = True) -> Circuit:
    pass