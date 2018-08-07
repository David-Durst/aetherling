from aetherling.modules.hydrate import Dehydrate, Hydrate
from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from mantle.common.sipo import SIPO
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from mantle.coreir.type_helpers import Term
from math import ceil


def DefineOneDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        stride: int,
        origin: int,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
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

    :return: A 1D Linebuffer
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

        name = "OneDimensionalLineBuffer_{}pxPerClock_{}windowWidth" \
               "_{}imgSize_{}outputStride_{}origin".format(
            pixel_per_clock, window_width, image_size, stride, origin
        )
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        window_per_active_clock = max(pixel_per_clock // stride, 1)
        IO = ['I', In(Array(pixel_per_clock, In(pixel_type))),
              'O', Out(Array(window_per_active_clock, Array(window_width, Out(pixel_type)))),
              'valid', Out(Bit)] + ClockInterface(has_ce=True)
        if not last_row:
            IO += ['next_row', Out(Array(pixel_per_clock, Bit))]

        @classmethod
        def definition(cls):

            # make a separate set of sipos for each bit
            type_size_in_bits = cirb.get_type(pixel_type, True).size
            shift_register = MapParallel(cirb, pixel_per_clock, MapParallel(cirb, type_size_in_bits,
                                                                            SIPO(image_size // pixel_per_clock, 0,
                                                                                 has_ce=True)))
            # convert the types to and from bits
            # make a dehydrate for each pixel coming in each clock
            type_to_bits = MapParallel(cirb, pixel_per_clock, Dehydrate(cirb, pixel_type))
            # make a hydrate for each pixel coming out each clock, for each pixel in window
            bits_to_type = MapParallel(cirb, cls.window_per_active_clock,
                                       MapParallel(cirb, window_width, Hydrate(cirb, pixel_type)))

            # reverse the pixels per clock. Since greater index_in_shift_register
            # mean earlier inputted pixels, also want greater current_shift_register
            # to mean earlier inputted pixels. This accomplishes that by making
            # pixels earlier each clock go to higher number shift register
            wire(cls.I[::-1], type_to_bits.I)
            wire(type_to_bits.out, shift_register.I)
            wire(bits_to_type.out, cls.O)
            for i in range(pixel_per_clock):
                for j in range(type_size_in_bits):
                    wire(cls.CE, shift_register.CE[i][j])

            # wire up each window, walking first across parallel inputs (across shift registers)
            # then to next entry of shift registers
            current_shift_register = 0
            index_in_shift_register = 0

            # since current_shift_register and index_in_shift_register form a
            # 2D shape where current_shift_registers is inner dimension and
            # index_in_shift_register is outer, these functions make the
            # 2D coordinates into 1D coordinates that match the 1D image
            def get_shift_register_location_in_1D_coordinates() -> int:
                return (index_in_shift_register * pixel_per_clock +
                        current_shift_register)

            def set_shift_register_location_using_1D_coordinates(location: int) -> int:
                nonlocal current_shift_register, index_in_shift_register
                index_in_shift_register = location // pixel_per_clock
                current_shift_register = location % pixel_per_clock

            used_coordinates = set()

            for current_window_index in range(cls.window_per_active_clock):
                # stride is handled by wiring if there are multiple windows emitted per clock,
                # aka if stride is less than number of pixels per clock.
                # In this case, multiple windows are emitted but they must be overlapped
                # less than normal
                strideMultiplier = stride if stride < pixel_per_clock else 1
                set_shift_register_location_using_1D_coordinates(
                    # the first window has the highest location as the oldest inputted pixel
                    # accepted will be in the highest index register in the shift register
                    strideMultiplier * (cls.window_per_active_clock - current_window_index - 1) +
                    # handle origin across multiple clocks by changing valid, but within a single clock
                    # need to adjust where the windows start
                    ((origin * -1) % pixel_per_clock)
                )
                # inverse index in a window as shift_register outputs are reverse of image order.
                # The lowest index outputs of shift_register are farthest to right
                # in input image.
                for index_in_window in range(window_width)[::-1]:
                    # can't wire up directly as have dimension for bits per type in between dimensions
                    # for px ber clock and number of entries in SIPO, so need to iterate here
                    for bit_index in range(type_size_in_bits):
                        wire(shift_register.O[current_shift_register][bit_index][index_in_shift_register],
                             bits_to_type.I[current_window_index][index_in_window][bit_index])

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
            for pixel_index in range(image_size):
                if pixel_index in used_coordinates:
                    continue
                set_shift_register_location_using_1D_coordinates(pixel_index)
                term = Term(cirb, type_size_in_bits)
                for bit_index in range(type_size_in_bits):
                    wire(term.I[bit_index],
                         shift_register.O[current_shift_register][bit_index][index_in_shift_register])

            # valid when the maximum coordinate used (minus origin, as origin can in
            # invalid space when emitting) gets data
            # add 1 here as coordinates are 0 indexed, and the denominator of this
            # fraction is the last register accessed
            # would add 1 outside fraction as it takes 1 clock for data
            # to get through registers but won't as 0 indexed
            valid_counter_max_value = ceil((max(used_coordinates) + 1 + origin) /
                                            pixel_per_clock)


            # add 1 as sizedcounter counts to 1 less than the provided max
            valid_counter = SizedCounterModM(valid_counter_max_value + 1, has_ce=True)

            valid_counter_max_instance = DefineCoreirConst(len(valid_counter.O),
                                                           valid_counter_max_value)()

            wire(enable(bit(cls.CE) &
                        (valid_counter.O < valid_counter_max_instance.out)), valid_counter.CE)

            # if stride is greater than pixels_per_clock, then need a stride counter as
            # not active every clock. Invalid clocks create striding in this case
            if stride > pixel_per_clock:

                stride_counter = SizedCounterModM(stride // pixel_per_clock, has_ce=True)
                stride_counter_0 = DefineCoreirConst(len(stride_counter.O),0)()

                wire(enable((stride_counter.O == stride_counter_0.out) &
                            (valid_counter.O == valid_counter_max_instance.out)),
                     cls.valid)

                # only increment stride if trying to emit data this clock cycle
                wire(valid_counter.O == valid_counter_max_instance.out, stride_counter.CE)

            else:
                wire((valid_counter.O == valid_counter_max_instance.out), cls.valid)



    return _LB


def OneDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixel_per_clock: int,
        window_width: int,
        image_size: int,
        output_stride: int,
        origin: int,
        last_row: bool = True) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
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


    Restrictions:
    1. image_size % pixel_per_clock == 0
    2. image_size % stride == 0
    3. stride % pixel_per_clock == 0 OR pixel_per_clock % stride == 0
    4. window_width > |origin|
    5. origin <= 0
    6. window_width < image_size

    :return: A 1D Linebuffer
    """
    return DefineOneDimensionalLineBuffer(
        cirb,
        pixel_type,
        pixel_per_clock,
        window_width,
        image_size,
        output_stride,
        origin,
        last_row
    )()
