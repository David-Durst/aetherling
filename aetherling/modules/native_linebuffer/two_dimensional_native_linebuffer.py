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
        pixels_per_row_per_clock: int,
        rows_of_pixels_per_clock: int,
        window_cols: int,
        window_rows: int,
        image_cols: int,
        image_rows: int,
        stride_cols: int,
        stride_rows: int,
        origin_cols: int,
        origin_rows: int,
        first_row: bool = True,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
    :param pixels_per_row_per_clock: The number of pixels per row of the image the linebuffer
    receives as input each clock.
    :param rows_of_pixels_per_clock: The number of rows of pixels of the image the linebuffer
    receives as input each clock.
    :param window_cols: The number of columns in the stencils that are emitted
    :param window_rows: The number of rows in the stencils that are emitted
    :param image_cols: The number of columns in the 2D input image
    :param image_rows: The number of rows in the 2D input image
    :param stride_cols: The distance between origins of consecutive stencils in number of pixels
    in the columns dimension. A stride of 1 means that successive stencils have origins in adjacent
    columns.
    :param stride_rows: The distance between origins of consecutive stencils in number of pixels
    in the rows dimension. A stride of 1 means that successive stencils have origins in adjacent
    rows.
    :param origin_cols: The column of the first pixel of the first window relative to
    the top left corner of the image
    :param origin_rows: The row of the first pixel of the first window relative to
    the top left corner of the image
    :param first_row: True if this is the first 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should not set this unless they understand the internals of the LineBuffer.
    Its used for determining when to reverse inputs to convert image coordinate to internal coordinates.
    :param last_row: True if this is the last 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should probably leave this to true. Its used for adding extra ports when putting these in
    larger matrices.


    Restrictions:

    1. image_cols % pixels_per_row_per_clock == 0
    2. image_rows % rows_of_pixels_per_clock == 0
    3. if rows_of_pixels_per_clock > 1, then image_cols == pixels_per_row_per_clock
    4. image_cols % stride_cols == 0
    5. image_rows % stride_rows == 0
    6. stride_cols % pixels_per_row_per_clock == 0 OR pixels_per_row_per_clock % stride_cols == 0
    7. stride_rows % rows_of_pixels_per_clock == 0 OR rows_of_pixels_per_clock % stride_rows == 0
    8. window_cols > |origin_cols|
    9. window_rows > |origin_rows|
    10. origin_cols <= 0
    11. origin_rows <= 0
    12. window_cols - origin_cols < image_cols
    13. window_rows - origin_rows < image_rows

    :return: A 2D Linebuffer with ports I, O, valid, CE, and next_row (if last_row false)
    """
    class _LB(Circuit):
        # this is necessary so that get same number of pixels in every
        # clock, don't have a weird ending with only 1 valid input pixel
        if image_size % pixel_per_clock != 0:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_size {} not divisiable by"
                            "pixel_per_clock {}".format(image_size,
                                                        pixel_per_clock))
        # the average number of output windows per clock = px per clock
        # / stride, this must be integer or reciprocal of one so that
        # easier to map/ underutil rest of system, otherwise
        # have a weirdly utilized downstream system that is only
        # partially used on some clocks
        if stride % pixel_per_clock != 0 and pixel_per_clock % stride != 0:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: output_stride {} not divisiable by"
                            "pixel_per_clock {} nor vice-verse. One of them must"
                            "be divisble by the other.".format(stride,
                                                               pixel_per_clock))
        # stride == downsample amount, this requires a cleanly divisible downsample
        if image_size % stride != 0:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_size {} not divisiable by"
                            "output_stride {}".format(image_size,
                                                      stride))

        # origin must be less than window, and can only be in one direction
        # if greater than window, then entire first window would be garbage,
        # which is meaningless
        # origin can't go into image as that is just crop, unsupported
        # functionality
        if abs(origin) >= window_width:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: |origin| {} greater than or equal to"
                            "window width {}".format(abs(origin),
                                                     window_width))
        if origin > 0:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: origin {} greater than"
                            "0".format(origin))

        # need window width plus origin outputs, if smaller than image,
        # this is a weird edge case that I don't want to deal with and
        # the user shouldn't be using a linebuffer for, because only 1
        # window output per image
        if window_width - origin >= image_size:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: window width {} - origin {} "
                            "greater than or equal to image size {}"
                            .format(window_width, origin, image_size))

        name = "OneDimensionalLineBuffer_{}pxPerClock_{}windowWidth" \
               "_{}imgSize_{}outputStride_{}origin".format(
            pixel_per_clock, window_width, image_size, stride, abs(origin)
        )
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        window_per_active_clock = max(pixel_per_clock // stride, 1)
        IO = ['I', In(Array(pixel_per_clock, In(pixel_type))),
              'O', Out(Array(window_per_active_clock, Array(window_width, Out(pixel_type)))),
              'valid', Out(Bit)] + ClockInterface(has_ce=True)
        if not last_row:
            IO += ['next_row', Out(Array(pixel_per_clock, Out(pixel_type)))]

        @classmethod
        def definition(cls):
            pass

    return _LB

def OneDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixels_per_row_per_clock: int,
        rows_of_pixels_per_clock: int,
        window_cols: int,
        window_rows: int,
        image_cols: int,
        image_rows: int,
        stride_cols: int,
        stride_rows: int,
        origin_cols: int,
        origin_rows: int,
        first_row: bool = True,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
    :param pixels_per_row_per_clock: The number of pixels per row of the image the linebuffer
    receives as input each clock.
    :param rows_of_pixels_per_clock: The number of rows of pixels of the image the linebuffer
    receives as input each clock.
    :param window_cols: The number of columns in the stencils that are emitted
    :param window_rows: The number of rows in the stencils that are emitted
    :param image_cols: The number of columns in the 2D input image
    :param image_rows: The number of rows in the 2D input image
    :param stride_cols: The distance between origins of consecutive stencils in number of pixels
    in the columns dimension. A stride of 1 means that successive stencils have origins in adjacent
    columns.
    :param stride_rows: The distance between origins of consecutive stencils in number of pixels
    in the rows dimension. A stride of 1 means that successive stencils have origins in adjacent
    rows.
    :param origin_cols: The column of the first pixel of the first window relative to
    the top left corner of the image
    :param origin_rows: The row of the first pixel of the first window relative to
    the top left corner of the image
    :param first_row: True if this is the first 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should not set this unless they understand the internals of the LineBuffer.
    Its used for determining when to reverse inputs to convert image coordinate to internal coordinates.
    :param last_row: True if this is the last 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should probably leave this to true. Its used for adding extra ports when putting these in
    larger matrices.


    Restrictions:

    1. image_cols % pixels_per_row_per_clock == 0
    2. image_rows % rows_of_pixels_per_clock == 0
    3. if rows_of_pixels_per_clock > 1, then image_cols == pixels_per_row_per_clock
    4. image_cols % stride_cols == 0
    5. image_rows % stride_rows == 0
    6. stride_cols % pixels_per_row_per_clock == 0 OR pixels_per_row_per_clock % stride_cols == 0
    7. stride_rows % rows_of_pixels_per_clock == 0 OR rows_of_pixels_per_clock % stride_rows == 0
    8. window_cols > |origin_cols|
    9. window_rows > |origin_rows|
    10. origin_cols <= 0
    11. origin_rows <= 0
    12. window_cols - origin_cols < image_cols
    13. window_rows - origin_rows < image_rows

    :return: A 2D Linebuffer with ports I, O, valid, CE, and next_row (if last_row false)
    """
    DefineTwoDimensionalLineBuffer(
        cirb,
        pixel_type,
        pixels_per_row_per_clock,
        rows_of_pixels_per_clock,
        window_cols,
        window_rows,
        image_cols,
        image_rows,
        stride_cols,
        stride_rows,
        origin_cols,
        origin_rows,
        first_row,
        last_row
    )()