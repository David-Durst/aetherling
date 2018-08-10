from aetherling.helpers.nameCleanup import cleanName
from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from aetherling.modules.sipo_any_type import SIPOAnyType
from aetherling.modules.term_any_type import TermAnyType
from math import ceil
from functools import reduce

def DefineAnyDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixels_per_clock: list,
        window_widths: list,
        image_sizes: list,
        strides: list,
        origins: list,
        first_row: bool = True,
        last_row: bool = True,
        top_layer: bool = True) -> Circuit:

    class _LB(Circuit):
        if len(pixels_per_clock) != len(window_widths) and \
            len(window_widths) != len(image_sizes) and \
            len(image_sizes) != len(strides) and \
            len(strides) != len(origins):
            raise Exception("not all inputs same dimensionality for any dimension linebuffer")

        if len(image_sizes) == 0:
            raise Exception("any dimensional linebuffer must have at least 1D inputs, parameters are empty lists")

        name_args = [cleanName(str(s)) for s in [pixel_type, pixels_per_clock, window_widths, image_sizes,
            strides, [abs(i) for i in origins], first_row, last_row]]
        name = "AnyDimensionalLineBuffer_{}type_{}pxPerClock_{}window" \
               "_{}img_{}stride_{}origin_{}firstRow_{}lastRow".format(*name_args)
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        windows_per_active_clock = max(pixels_per_clock[0] // strides[0], 1)


        top_layer_valid = ['valid', Out(Bit)] if top_layer else []
        IO = ['I', In(get_type_recursive(pixel_type, pixels_per_clock)),
              'O', Out(Array(windows_per_active_clock, get_type_recursive(pixel_type, window_widths)))] + \
              top_layer_valid + ClockInterface(has_ce=True)
        if not last_row:
            IO += ['next_row', Out(get_type_recursive(pixel_type, pixels_per_clock))]

        @classmethod
        def definition(cls):

            pixel_per_clock = pixels_per_clock[0]
            window_width = window_widths[0]
            image_size = image_sizes[0]
            stride = strides[0]
            origin = origins[0]


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

            # used coordinates tracks all the 2D coordinates used both for window outputs
            # and for emitting at the end of the row if connecting multiple 1D linebuffers
            used_coordinates = set()

            # just tracks the registers used for window outputs. This is used for computing
            # how many registers to make and how much memory can be SRAMs with only inputs
            # and outputs on the ends
            registers_to_window_outputs = []

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
                    used_coordinates.add((index_in_shift_register, current_shift_register))

                    registers_to_window_outputs.append(RegisterToWindowMapping(
                        current_shift_register, index_in_shift_register,
                        current_window_index, index_in_window
                    ))

                    set_shift_register_location_using_1D_coordinates(
                        get_shift_register_location_in_1D_coordinates() + 1
                    )

            # now that know how many registers to use, actually make them and then wire them up
            shift_register = make_two_dimensional_set_of_lower_dimensional_linebuffer_as_shift_registers(
                cirb,
                pixel_type,
                pixels_per_clock,
                window_widths,
                image_sizes,
                strides,
                origins
                #registers_to_window_outputs # use this once I have a rowbuffer and
                                             # worth it to separate registers and SRAMS
            )

            for mapping in registers_to_window_outputs:
                wire(shift_register.O[mapping.current_shift_register][mapping.index_in_shift_register],
                     cls.O[mapping.current_window_index][mapping.index_in_window])

            # reverse the pixels per clock. Since greater index_in_shift_register
            # mean earlier inputted pixels, also want greater current_shift_register
            # to mean earlier inputted pixels. This accomplishes that by making
            # pixels earlier each clock go to higher number shift register
            if first_row:
                wire(cls.I[::-1], shift_register.I)
            else:
                # don't need to reverse if not first row as prior rows have already done reversing
                wire(cls.I, shift_register.I)

            for i in range(pixel_per_clock):
                wire(cls.CE, shift_register.CE[i])

            # if not last row, have output ports for ends of all shift_registers so next
            # 1D can accept them
            if not last_row:
                index_in_shift_register = image_size // pixel_per_clock - 1
                for current_shift_register in range(pixel_per_clock):
                    wire(shift_register.O[current_shift_register][index_in_shift_register],
                         cls.next_row[current_shift_register])
                    used_coordinates.add((index_in_shift_register, current_shift_register))

            # wire up all non-used coordinates to terms
            for sr in range(pixel_per_clock):
                for sr_index in range(image_size // pixel_per_clock):
                    if (sr_index, sr) in used_coordinates:
                        continue
                    term = TermAnyType(cirb, pixel_type)
                    wire(shift_register.O[sr][sr_index], term.I)

            # make a counter for valid only if this is 1D, otherwise let lower dimensions handle valid
            if len(pixels_per_clock) == 1:
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
                    stride_counter_0 = DefineCoreirConst(len(stride_counter.O), 0)()

                    wire(enable((stride_counter.O == stride_counter_0.out) &
                                (valid_counter.O == valid_counter_max_instance.out)),
                         cls.valid)

                    # only increment stride if trying to emit data this clock cycle
                    wire(valid_counter.O == valid_counter_max_instance.out, stride_counter.CE)

                else:
                    wire((valid_counter.O == valid_counter_max_instance.out), cls.valid)
            else:
                print(pixels_per_clock)
                wire(shift_register.valid, cls.valid)

    return _LB


def AnyDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixel_per_clock: list,
        window_width: list,
        image_size: list,
        stride: list,
        origin: list,
        first_row: bool = True,
        last_row: bool = True,
        top_layer: bool = True) -> Circuit:

    return DefineAnyDimensionalLineBuffer(
        cirb,
        pixel_type,
        pixel_per_clock,
        window_width,
        image_size,
        stride,
        origin,
        first_row,
        last_row,
        top_layer
    )()


def make_two_dimensional_set_of_lower_dimensional_linebuffer_as_shift_registers(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixels_per_clock: list,
        window_widths: list,
        image_sizes: list,
        strides: list,
        origins: list) -> Circuit:
    """
    make a series of linebuffers that are all one dimenision lower than the one trying to create
    the first and last linebuffers don't expose the internal next_row port
    """
    head_pixel_per_clock, *tail_pixel_per_clock = pixels_per_clock
    head_window_width, *tail_window_width = window_widths
    head_image_size, *tail_image_size = image_sizes
    head_stride, *tail_stride = strides
    head_origin, *tail_origin = origins
    number_of_linebuffers_in_sequence = head_image_size // head_pixel_per_clock

    # don't make linebuffer, just return SIPO if 1D case
    if len(image_sizes) == 1:
        return MapParallel(cirb, head_pixel_per_clock,
                           SIPOAnyType(cirb, head_image_size // head_pixel_per_clock,
                                       pixel_type, 0, has_ce=True))


    lower_dimensional_linebuffers = [
        [
            AnyDimensionalLineBuffer(cirb,
                                     pixel_type,
                                     tail_pixel_per_clock,
                                     tail_window_width,
                                     tail_image_size,
                                     tail_stride,
                                     tail_origin,
                                     i == 0,
                                     i == number_of_linebuffers_in_sequence - 1,
                                     False)
            for i in range(number_of_linebuffers_in_sequence)
        ]
        for _ in range(head_pixel_per_clock)
    ]

    # wire up the next_rows
    for sequence_of_linebuffers in lower_dimensional_linebuffers:
        for i in range(len(sequence_of_linebuffers) - 1):
            wire(sequence_of_linebuffers[i].next_row, sequence_of_linebuffers[i+1].I)
            wire(sequence_of_linebuffers[i].next_row, sequence_of_linebuffers[i+1].I)

    # making a fake .O port that collects the .O ports of all linebuffers
    lower_dimensional_linebuffers.O = [[lb.O for lb in lb_sequence] for lb_sequence in lower_dimensional_linebuffers]
    lower_dimensional_linebuffers.CE = [lb_sequence[0].CE for lb_sequence in lower_dimensional_linebuffers]

    # for origin of more than 1 clock cycle, handle the early clock cycle by ignoring the valids of the later
    # linebuffers. just valid when earlier linebuffers are ready.
    all_valids = [lb_sequence[0].valid for lb_sequence in lower_dimensional_linebuffers]
    early_origin_clocks = head_origin // head_pixel_per_clock
    used_valids = all_valids[:early_origin_clocks]
    ignored_valids = all_valids[early_origin_clocks:]

    # wire up all non-used valids to terms
    for v in ignored_valids:
        term = TermAnyType(cirb, Bit)
        wire(v, term.I)

    lower_dimensional_linebuffers_valid = reduce(lambda valid0, valid1: valid0 & valid1, used_valids)

    # if stride is greater than pixel per clock in this dimension, then need a counter to not be valid
    if head_stride > head_pixel_per_clock:
        # how many times is the lower dimension valid before this dimension increments by 1
        number_of_lower_dimension_valids_per_increment = lower_dimensional_linebuffers[0][0].windows_per_active_clock

        # this slows the main stride counter to only increment once per number_of_lower_dimension_valids_per_increment
        stride_per_lower_dimension_counter = SizedCounterModM(number_of_lower_dimension_valids_per_increment, has_ce=True)
        stride_per_lower_dimension_max = DefineCoreirConst(len(stride_per_lower_dimension_counter.O),
                                                           number_of_lower_dimension_valids_per_increment - 1)()
        stride_counter = SizedCounterModM(head_stride // head_pixel_per_clock, has_ce=True)
        stride_counter_0 = DefineCoreirConst(len(stride_counter.O), 0)()

        wire(lower_dimensional_linebuffers_valid, stride_per_lower_dimension_counter.CE)

        wire(enable(stride_per_lower_dimension_max.O == stride_per_lower_dimension_counter.O),
             stride_counter.CE)

        wire(lower_dimensional_linebuffers_valid & (stride_counter_0.O == stride_counter.O),
            lower_dimensional_linebuffers.valid)
    else:
        lower_dimensional_linebuffers.valid = lower_dimensional_linebuffers_valid


    return lower_dimensional_linebuffers


def get_type_recursive(pixel_type: Kind, dimensions: list):
    if len(dimensions) == 0:
        return pixel_type
    else:
        return Array(dimensions[0], get_type_recursive(pixel_type, dimensions[1:]))

class RegisterToWindowMapping():
    def __init__(self, current_shift_register: int, index_in_shift_register: int,
                 current_window_index: int, index_in_window):
        self.current_shift_register = current_shift_register
        self.index_in_shift_register = index_in_shift_register
        self.current_window_index = current_window_index
        self.index_in_window = index_in_window