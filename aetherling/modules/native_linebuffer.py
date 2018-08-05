from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from mantle.common.sipo import SIPO
from mantle.common.compare import ULT
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from mantle.coreir.type_helpers import Term


def DefineOneBitOneDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        stride: int,
        origin: int,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_per_clock: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the first pixel of the first window relative to
    the top left corner of the image
    :param last_row: True if this is the last 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should probably leave this to true. Its used for adding extra ports when putting these in
    larger matrices.

    Restrictions:
    1. image_size % pixel_per_clock == 0
    2. image_size % stride == 0
    3. stride % pixel_per_clock == 0 OR pixel_per_clock % stride == 0
    4. window_width > |origin|
    5. origin <= 0
    6. window_width < image_size

    :return: A 1D, 1 Bit Linebuffer
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

        # need window width outputs, if smaller than image, this is a weird
        # edge case that I don't want to deal with and the user shouldn't be
        # using a linebuffer for, because only 1 window output per image
        if window_width > image_size:
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: origin {} greater than"
                            "window width {}".format(origin,
                                                     window_width))

        name = "OneBitOneDimensionalLineBuffer_{}pxPerClock_{}windowWidth" \
               "_{}imgSize_{}outputStride_{}origin".format(
            pixel_per_clock, window_width, image_size, stride, origin
        )
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        window_per_active_clock = max(pixel_per_clock // stride, 1)
        IO = ['I', In(Array(pixel_per_clock, Bit)),
              'O', Out(Array(window_per_active_clock, Array(window_width, Bit))),
              'valid', Out(Bit)] + ClockInterface(has_ce=True)
        if not last_row:
            IO += ['next_row', Out(Array(pixel_per_clock, Bit))]

        @classmethod
        def definition(cls):

            shift_register = MapParallel(cirb, pixel_per_clock,
                                         SIPO(image_size // pixel_per_clock, 0, has_ce=True))
            wire(cls.I, shift_register.I)
            for i in range(pixel_per_clock):
                wire(cls.CE, shift_register.CE[i])

            # wire up each window, walking first across parallel inputs (across shift registers)
            # then to next entry of shift registers
            current_shift_register = 0
            index_in_shift_register = 0

            # since current_shift_register and index_in_shift_register form a
            # 2D shape where I'm walking across shift_registers first and then in
            # across the shift_register's indexes, these functions make the
            # 2D coordinates into 1D coordinates that match the 1D image
            def get_shift_register_location_in_1D_coordinates() -> int:
                return (current_shift_register * pixel_per_clock +
                        index_in_shift_register)
            def set_shift_register_location_using_1D_coordinates(location: int) -> int:
                nonlocal current_shift_register, index_in_shift_register
                index_in_shift_register = location // pixel_per_clock
                current_shift_register = location % pixel_per_clock
            used_coordinates = set()


            # do the windows in reverse, as the last pixel accepted will be in the first
            # register in the shift register
            for current_window_index in range(cls.window_per_active_clock)[::-1]:
                set_shift_register_location_using_1D_coordinates(
                    stride * (cls.window_per_active_clock - current_window_index - 1)
                )
                # wire up a window while still entries in it
                for index_in_window in range(window_width):
                    # weird window index as shift_register outputs are reverse of image order
                    # as lowest index outputs of shift_register are farthest to right
                    # in input image. window_width - index_in_window - 1 does reversing for
                    # the output
                    wire(cls.O[current_window_index][window_width - index_in_window - 1],
                         shift_register.O[current_shift_register][index_in_shift_register])
                    used_coordinates.add(get_shift_register_location_in_1D_coordinates())

                    set_shift_register_location_using_1D_coordinates(
                        get_shift_register_location_in_1D_coordinates() + 1
                    )

            # if not last row, have output ports for ends of all shift_registers so next
            # 1D can accept them
            if not last_row:
                for i in range(pixel_per_clock):
                    current_location = (len(shift_register.O) - 1) * pixel_per_clock + i
                    set_shift_register_location_using_1D_coordinates(current_location)
                    wire(cls.next_row,
                         shift_register.O[current_shift_register][index_in_shift_register])
                    used_coordinates.add(current_location)

            # wire up all non-used coordinates to terms
            for i in range(image_size):
                if i in used_coordinates:
                    continue
                set_shift_register_location_using_1D_coordinates(i)
                term = Term(cirb, 1)
                wire(term.I[0], shift_register.O[current_shift_register][index_in_shift_register])

            # valid when the maximum coordinate used (minus origin, as origin can in
            # invalid space when emitting) gets data
            # add 1 to valid as it takes 1 clock for data to get through registers
            valid_counter = SizedCounterModM(max(used_coordinates) + 2 - origin, has_ce=True)

            valid_counter_max = DefineCoreirConst(len(valid_counter.O),
                                                  max(used_coordinates) + 1 - origin)()

            wire(enable(bit(cls.CE) &
                 (valid_counter.O < valid_counter_max.out)), valid_counter.CE)
            wire((valid_counter.O == valid_counter_max.out), cls.valid)

    return _LB


def OneBitOneDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        output_stride: int,
        origin: int,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_per_clock: The number of pixels (bits in this case) that the
    linebuffer receives as input each clock.
    :param window_width: The size of the stencils that are emitted
    :param image_size: The size of the complete, 1D image
    :param output_stride: The distance between origins of consecutive stencils
    in terms of numbers of pixels. A stride of 1 means that they are next
    to each other.
    :param origin: The index of the currently provided pixel
    :param last_row: True if this is the last 1D row in a 2D matrix or the only one in a 2D matrix.
    Users should probably leave this to true. Its used for adding extra ports when putting these in
    larger matrices.

    :return:
    """
    return DefineOneBitOneDimensionalLineBuffer(
        cirb,
        pixel_per_clock,
        window_width,
        image_size,
        output_stride,
        origin,
        last_row
    )()
