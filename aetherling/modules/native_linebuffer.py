from magma import *
from mantle.common.sipo import SIPO

@cache_definition
def DefineOneBitOneDimensionalLineBuffer(pixel_per_clock: int,
                                         window_width: int,
                                         image_size: int,
                                         output_stride: int,
                                         origin: int) -> Circuit:
    """
    :param pixel_per_clock: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param output_stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the first pixel of the first window in the
     image
    :return:
    """
    class _LB(Circuit):
        if window % pixel_per_clock != 0:

        name = "OneBitOneDimensionalLineBuffer_{}pxPerClock_{}windowWidth" \
               "_{}imgSize_{}outputStride_{}origin".format(
            pixel_per_clock, window_width, image_size, output_stride, origin
        )
        IO = ['I', In(Array(pixel_per_clock, Bit)),
              'O', Out(Array(window_width, Bit)),
              'valid', Out(Bit)] + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):

            shift_register = SIPO(window_width)
            wire(shift_register.I, cls.I)
            wire(shift_register.O, cls.O)

    return _LB

def OneBitOneDimensionalLineBuffer(pixel_per_clock: int,
                                         window_width: int,
                                         image_size: int,
                                         output_stride: int,
                                         origin: int) -> Circuit:
    """
    :param pixel_per_clock: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param output_stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the currently provided pixel
    :return:
    """
    return DefineOneBitOneDimensionalLineBuffer(
        pixel_per_clock,
        window_width,
        image_size,
        output_stride,
        origin
    )()