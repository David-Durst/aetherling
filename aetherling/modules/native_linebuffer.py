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
    :param origin: The index of the first pixel of the first window relative to

     image
    :return:
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
        if output_stride % pixel_per_clock != 0 and pixel_per_clock % output_stride != 0:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: output_stride {} not divisiable by"
                            "pixel_per_clock {} nor vice-verse. One of them must"
                            "be divisble by the other.".format(output_stride,
                                                        pixel_per_clock))
        # stride == downsample amount, this requires a cleanly divisible downsample
        if image_size % output_stride != 0:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_size {} not divisiable by"
                            "output_stride {}".format(image_size,
                                                        output_stride))


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