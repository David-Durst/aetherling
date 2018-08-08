from aetherling.helpers.nameCleanup import cleanName
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
        first_row: bool = True,
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
               "_{}img_{}stride_{}origin_{}firstRow_{}lastRow".format(
            cleanName(str(pixel_type)), pixel_per_clock, window_width, image_size,
            stride, abs(origin), first_row, last_row
        )
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        windows_per_active_clock = max(pixel_per_clock // stride, 1)
        IO = ['I', In(Array(pixel_per_clock, In(pixel_type))),
              'O', Out(Array(windows_per_active_clock, Array(window_width, Out(pixel_type)))),
              'valid', Out(Bit)] + ClockInterface(has_ce=True)
        if not last_row:
            IO += ['next_row', Out(Array(pixel_per_clock, Out(pixel_type)))]

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
            bits_to_type = MapParallel(cirb, cls.windows_per_active_clock,
                                       MapParallel(cirb, window_width, Hydrate(cirb, pixel_type)))

            # reverse the pixels per clock. Since greater index_in_shift_register
            # mean earlier inputted pixels, also want greater current_shift_register
            # to mean earlier inputted pixels. This accomplishes that by making
            # pixels earlier each clock go to higher number shift register
            if first_row:
                wire(cls.I[::-1], type_to_bits.I)
            else:
                # don't need to reverse if not first row as prior rows have already done reversing
                wire(cls.I, type_to_bits.I)
            wire(type_to_bits.out, shift_register.I)
            wire(bits_to_type.out, cls.O)
            for i in range(pixel_per_clock):
                for j in range(type_size_in_bits):
                    wire(cls.CE, shift_register.CE[i][j])

            # these two variables provide a 2D coordinate system for the SIPOs.
            # the inner dimension is current_shift_register
            # the outer dimension is index_in_shift_register
            # greater values in current_shift_register are inputs from older clocks
            # greater values in index_in_shift_register are inputs from lower index
            # values in the inputs in a single clock (due to above cls.I, type_to_bits reversing)
            # the index_in_shift_register is reversed so that bigger number always
            # means lower indexed value in the input image. For example, if asking
            # for location 0 with a 2 px per clock, 3 window width, then the
            # 2D location is index_in_shift_register = 1, current_shift_register = 1
            # and walking back in 2D space as increasing 1D location.
            current_shift_register = 0
            index_in_shift_register = 0

            # since current_shift_register and index_in_shift_register form a
            # 2D shape where current_shift_registers is inner dimension and
            # index_in_shift_register is outer, get_shift_register_location_in_1D_coordinates
            # and set_shift_register_location_using_1D_coordinates  convert between
            # 2D coordinates in the SIPOs and 1D coordinates in the 1D image

            # To do the reversing of 1D coordinates, need to find the oldest pixel that should be output,
            # ignoring origin as origin doesn't impact this computation.
            # This is done by finding the number of relevant pixels for outputting and adjusting it
            # so that it aligns with the number of pixels per clock cycle.
            # That coordinates position is treated as a 0 in the reverse coordinates
            # and requested coordinates (going in the opposite direction) are reversed
            # and adjusted to fit the new coordinate system by subtracting their values
            # from the 0's value in the original, forward coordinate system.

            # need to be able to handle situations with swizzling. Swizzling is
            # where a pixel inputted this clock is not used until next clock.
            # This is handled by wiring up in reverse order. If a pixel is inputted
            # in a clock but not used, it will have a high 1D location as it will be
            # one of the first registers in the first index_in_shift_register.
            # The swizzled pixel's large 1D location ensures it isn't wired directly
            # to an output

            # get needed pixels (ignoring origin as that can be garbage)
            # to determine number of clock cycles needed to satisfy input
            if cls.windows_per_active_clock == 1:
                needed_pixels = window_width
            else:
                needed_pixels = window_width + stride * (cls.windows_per_active_clock - 1)

            # get the maximum 1D coordinate when aligning needed pixels to the number
            # of pixels per clock
            if needed_pixels % pixel_per_clock == 0:
                oldest_needed_pixel_forward_1D_coordinates = needed_pixels
            else:
                oldest_needed_pixel_forward_1D_coordinates = ceil(needed_pixels / pixel_per_clock) * \
                                                             pixel_per_clock

            # adjust by 1 for 0 indexing
            oldest_needed_pixel_forward_1D_coordinates -= 1

            def get_shift_register_location_in_1D_coordinates() -> int:
                return oldest_needed_pixel_forward_1D_coordinates - \
                       (index_in_shift_register * pixel_per_clock +
                        current_shift_register)

            def set_shift_register_location_using_1D_coordinates(location: int) -> int:
                nonlocal current_shift_register, index_in_shift_register
                location_reversed_indexing = oldest_needed_pixel_forward_1D_coordinates - location
                index_in_shift_register = location_reversed_indexing // pixel_per_clock
                current_shift_register = location_reversed_indexing % pixel_per_clock

            used_coordinates = set()

            for current_window_index in range(cls.windows_per_active_clock):
                # stride is handled by wiring if there are multiple windows emitted per clock,
                # aka if stride is less than number of pixels per clock.
                # In this case, multiple windows are emitted but they must be overlapped
                # less than normal
                strideMultiplier = stride if stride < pixel_per_clock else 1
                set_shift_register_location_using_1D_coordinates(
                    strideMultiplier * current_window_index +
                    # handle origin across multiple clocks by changing valid, but within a single clock
                    # need to adjust where the windows start
                    # need neg conversion twice due to issues taking mod of negative number
                    ((origin * -1) % pixel_per_clock * -1)
                )
                for index_in_window in range(window_width):
                    # can't wire up directly as have dimension for bits per type in between dimensions
                    # for px ber clock and number of entries in SIPO, so need to iterate here
                    for bit_index in range(type_size_in_bits):
                        wire(shift_register.O[current_shift_register][bit_index][index_in_shift_register],
                             bits_to_type.I[current_window_index][index_in_window][bit_index])

                    used_coordinates.add((index_in_shift_register, current_shift_register))

                    set_shift_register_location_using_1D_coordinates(
                        get_shift_register_location_in_1D_coordinates() + 1
                    )

            # if not last row, have output ports for ends of all shift_registers so next
            # 1D can accept them
            if not last_row:
                index_in_shift_register = image_size // pixel_per_clock - 1
                hydrate_interrow_pixels = MapParallel(cirb, pixel_per_clock,
                                                    Hydrate(cirb, pixel_type))
                wire(hydrate_interrow_pixels.out, cls.next_row)
                for i in range(pixel_per_clock):
                    current_shift_register = i
                    for bit_index in range(type_size_in_bits):
                        wire(shift_register.O[current_shift_register][bit_index][index_in_shift_register],
                             hydrate_interrow_pixels[current_shift_register][bit_index])
                    used_coordinates.add((index_in_shift_register, current_shift_register))

            # wire up all non-used coordinates to terms
            for sr in range(pixel_per_clock):
                for sr_index in range(image_size // pixel_per_clock):
                    if (sr_index, sr) in used_coordinates:
                        continue
                    term = Term(cirb, type_size_in_bits)
                    for bit_index in range(type_size_in_bits):
                        wire(term.I[bit_index],
                             shift_register.O[sr][bit_index][sr_index])

            # valid when the maximum coordinate used (minus origin, as origin can in
            # invalid space when emitting) gets data
            # add 1 here as coordinates are 0 indexed, and the denominator of this
            # fraction is the last register accessed
            # would add 1 outside fraction as it takes 1 clock for data
            # to get through registers but won't as 0 indexed
            valid_counter_max_value = ceil((oldest_needed_pixel_forward_1D_coordinates + 1 + origin) /
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
        first_row: bool = True,
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
        cirb,
        pixel_type,
        pixel_per_clock,
        window_width,
        image_size,
        output_stride,
        origin,
        first_row,
        last_row
    )()
