from aetherling.helpers.nameCleanup import cleanName
from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from aetherling.modules.native_linebuffer.any_dimensional_native_linebuffer import AnyDimensionalLineBuffer
from math import ceil

@cache_definition
def DefineOneDimensionalLineBuffer(
        pixel_type: Kind,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        stride: int,
        origin: int) -> Circuit:
    """
    :param pixel_type: the type of each pixel. A type of Array[3, Array[8, Bit])] is for
    3 color channel, 8 bits per channel.
    :param pixel_per_clock: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the first pixel of the first window relative to
    the first pixel of the image

    Restrictions:
    1. image_size % pixel_per_clock == 0
    2. image_size % stride == 0
    3. stride % pixel_per_clock == 0 OR pixel_per_clock % stride == 0
    4. window_width > |origin|
    5. origin <= 0
    6. window_width - origin < image_size

    :return: A 1D Linebuffer with ports I, O, valid, and CE
    """

    class _LB(Circuit):
        if image_size % pixel_per_clock != 0:
            reason = """
            this is necessary so that input a complete image with the same number of input
            pixels in every clock and don't have a weird ending with only 1 valid input pixel
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_size {} not divisiable by"
                            "pixel_per_clock {}. \n Reason: {}".format(image_size,
                                                                       pixel_per_clock,
                                                                       reason))

        if image_size % stride != 0:
            reason = "stride is downsample factor, so this ensures a downsample factor" \
                     "that cleanly divides the image size"
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_size {} not divisible by"
                            "stride {}. \n Reason: {}".format(image_size,
                                                              stride,
                                                              reason))

        if stride % pixel_per_clock != 0 and pixel_per_clock % stride != 0:
            reason = """
            average number of output windows per clock = px per clock
            / stride. Number of output windows per clock must be integer or 
            reciprocal of one so that throughput is an easier factor to manipulate 
            with map/underutil.
            
            Otherwise throughput is a weird fraction and the downstream system is
            either only partially used on some clocks or the sequence length
            is multiplied by a weird factor that makes the rational number
            throughput become an integer.            
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: stride {} not divisible by"
                            "pixel_per_clock {} nor vice-verse. One of them must"
                            "be divisible by the other. \n Reason: {}".format(stride,
                                                                              pixel_per_clock,
                                                                              reason))


        if abs(origin) >= window_width:
            reason = """
            origin must be less than window. If abs(origin) was greater
            than window, then entire first window would be garbage
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: |origin| {} greater than or equal to"
                            "window width {} \n Reason: {}".format(abs(origin),
                                                                   window_width,
                                                                   reason))
        if origin > 0:
            reason = """
            origin can't go into image. That would be cropping the first pixels of the image
            and linebuffer doesn't do cropping.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: origin {} greater than 0. \n Reason: {}".format(origin, reason))

        if window_width - origin >= image_size:
            reason = """
            need window width plus abs(origin) outputs to do wiring.
            If the image is smaller than this, will have issues with
            internal wiring. Additionally, the linebuffer isn't
            used for images that are small enough to be processed
            in one or a few clock cycles. This is a weird edge 
            case that I don't want to deal with and shouldn't occur
            in the real world.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: window_width {} - origin {} "
                            "greater than or equal to image_size {}. \n Reason: {}"
                            .format(window_width, origin, image_size, reason))

        name = "OneDimensionalLineBuffer_{}type_{}pxPerClock_{}window" \
               "_{}img_{}stride_{}origin".format(
            cleanName(str(pixel_type)), pixel_per_clock, window_width, image_size,
            stride, abs(origin)
        )
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        windows_per_active_clock = max(pixel_per_clock // stride, 1)
        IO = ['I', In(Array[pixel_per_clock, In(pixel_type)]),
              'O', Out(Array[windows_per_active_clock, Array[window_width, Out(pixel_type)]]),
              'valid', Out(Bit), 'ready', Out(Bit)] + ClockInterface(has_ce=True)

        @classmethod
        def definition(cls):
            lb = AnyDimensionalLineBuffer(
                pixel_type,
                [pixel_per_clock],
                [window_width],
                [image_size],
                [stride],
                [origin]
            )
            wire(cls.I, lb.I)
            wire(lb.O, cls.O)
            wire(cls.CE, lb.CE)
            wire(lb.valid, cls.valid)
            wire(cls.ready, 1)

    return _LB


def OneDimensionalLineBuffer(
        pixel_type: Kind,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        output_stride: int,
        origin: int,
        first_row: bool = True,
        last_row: bool = True) -> Circuit:
    """
    :param pixel_type: the type of each pixel. A type of Array[3, Array[8, Bit])] is for
    3 color channel, 8 bits per channel.
    :param pixel_per_clock: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the first pixel of the first window relative to
    the first pixel of the image
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

    :return: A 1D Linebuffer with ports I, O, valid, CE, and next_row (if last_row false)
    """
    return DefineOneDimensionalLineBuffer(
        pixel_type,
        pixel_per_clock,
        window_width,
        image_size,
        output_stride,
        origin
    )()
