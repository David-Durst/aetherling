from aetherling.helpers.nameCleanup import cleanName
from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.map_fully_parallel_sequential import DefineMapParallel
from aetherling.modules.sipo_any_type import SIPOAnyType
from aetherling.modules.term_any_type import TermAnyType
from math import ceil
from functools import reduce
from collections import namedtuple

def DefineAnyDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixels_per_clock: list,
        window_widths: list,
        image_sizes: list,
        strides: list,
        origins: list,
        first_row: bool = True,
        last_row: bool = True) -> Circuit:

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
        windows_per_active_clock = [max(p // s, 1) for (p, s) in zip(pixels_per_clock, strides)]

        IO = ['I', In(get_nested_type(pixel_type, pixels_per_clock)),
              'O', Out(get_nested_type(pixel_type, windows_per_active_clock + window_widths))] + \
             ['valid', Out(Bit)] + ClockInterface(has_ce=True)
        if not last_row:
            IO += ['next_row', Out(get_nested_type(pixel_type, pixels_per_clock)),
                   'next_row_valid', Out(Bit)]

        @classmethod
        def definition(cls):

            num_dimensions = len(pixels_per_clock)
            pixel_per_clock = pixels_per_clock[0]
            window_width = window_widths[0]
            image_size = image_sizes[0]
            stride = strides[0]
            origin = origins[0]

            shift_registers = create_parallel_shift_registers(cirb, cls.name, pixel_type,
                                            pixels_per_clock, image_sizes)

            def recursively_wire_input(lb_inputs, shift_register_inputs, dimension_depth):
                if dimension_depth == 1:
                    wire(lb_inputs[::-1], shift_register_inputs)
                else:
                    for i in range(len(lb_inputs)):
                        recursively_wire_input(lb_inputs[len(lb_inputs - i - 1)],
                                         shift_register_inputs[i], dimension_depth - 1)

            recursively_wire_input(cls.I, shift_registers.I, len(pixels_per_clock))


            # these two variables provide a 2N dimensional, where N is the number
            # of dimensions in the images. The dimensions using shift registers is
            # 2N dimensional as you need an extra dimension for each level of parallelism.
            # greater values in dimensions are earlier indexes in input image
            coordinates_2N_dimensional = [0] * (2*num_dimensions)

            # get_shift_register_location_in_ND_coordinates
            # and set_shift_register_location_using_ND_coordinates convert between
            # 2ND coordinates in the shift registers and ND coordinates in the ND image

            # To do the reversing of coordinates, need to find the oldest pixel that should be output,
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
            # in a clock but not used, it will have a high ND location as it will be
            # one of the first registers.
            # The swizzled pixel's large ND location ensures it isn't wired directly
            # to an output

            # get needed pixels (ignoring origin as that can be garbage)
            # to determine number of clock cycles needed to satisfy input
            oldest_needed_pixel_forward_ND_coordinates = [0] * num_dimensions

            for dimension in range(num_dimensions):
                if cls.windows_per_active_clock[dimension] == 1:
                    needed_pixels_cur_dim = window_widths[dimension]
                else:
                    needed_pixels_cur_dim = window_widths[dimension] + strides[dimension] * \
                                               (cls.windows_per_active_clock[dimension] - 1)

                # get the maximum 1D coordinate when aligning needed pixels to the number
                # of pixels per clock
                if needed_pixels % pixel_per_clock == 0:
                    oldest_needed_pixel_forward_ND_coordinates[dimension] = needed_pixels_cur_dim
                else:
                    oldest_needed_pixel_forward_ND_coordinates[dimension] = \
                        ceil(needed_pixels_cur_dim / pixels_per_clock[dimension]) * pixels_per_clock[dimension]

                # adjust by 1 for 0 indexing
                oldest_needed_pixel_forward_ND_coordinates[dimension] -= 1

            # this function all wrong, think its intermixing coordinates
            def get_shift_register_location_in_ND_coordinates(input_coordinates: list, dimension) -> int:
                current_dimension_coordinate = sum([pixel_per_clock * input_coordinates[] / image_sizes[dimension]
                return [current_dimension_coordinate] get_shift_register_location_in_ND_coordinates(
                    input_coordinates
                )

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

            for current_window_index in range(cls.windows_per_active_clock[0]):
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
                cls.name,
                pixel_type,
                pixels_per_clock,
                window_widths,
                image_sizes,
                strides,
                origins
                #registers_to_window_outputs # use this once I have a rowbuffer and
                                             # worth it to separate registers and SRAMS
            )()

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
                wire(cls.CE, shift_register.CE)

            # if not last row, have output ports for ends of all shift_registers so next
            # 1D can accept them
            # and a valid signal for when that port is ready
            if not last_row:
                index_in_shift_register = image_size // pixel_per_clock - 1
                for current_shift_register in range(pixel_per_clock):
                    wire(shift_register.O[current_shift_register][index_in_shift_register],
                         cls.next_row[current_shift_register])
                    used_coordinates.add((index_in_shift_register, current_shift_register))

                valid_next_counter = SizedCounterModM(image_size // pixel_per_clock, has_ce=True)
                valid_next_counter_max_instance = DefineCoreirConst(len(valid_next_counter.O),
                                                                    image_size // pixel_per_clock - 1)()
                wire(valid_next_counter.O == valid_next_counter_max_instance.out,
                     cls.next_row_valid)
                wire(enable((valid_next_counter.O < valid_next_counter_max_instance.out) &
                     bit(cls.CE)), valid_next_counter.CE)


            # wire up all non-used coordinates to terms
            for sr in range(pixel_per_clock):
                for sr_index in range(image_size // pixel_per_clock):
                    if (sr_index, sr) in used_coordinates:
                        continue
                    # .T.T gets the element type of the nested level .O[sr][sr_index]
                    # (i.e. if its Array(4, Array(3, Array(2, Bit))) port, .T.T gets Array(2, Bit)
                    term = TermAnyType(cirb, shift_register.O.T.T)
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
        last_row: bool = True) -> Circuit:

    return DefineAnyDimensionalLineBuffer(
        cirb,
        pixel_type,
        pixel_per_clock,
        window_width,
        image_size,
        stride,
        origin,
        first_row,
        last_row
    )()


def create_parallel_shift_registers(
        cirb: CoreIRBackend,
        parent_name: str,
        pixel_type: Kind,
        pixels_per_clock: list,
        image_sizes: list) -> Circuit:
    """
    Create the nested parallel, high dimensional shift registers
    """
    parallelized_shift_registers = define_n_dimensional_shift_registers(
        cirb,
        parent_name,
        pixel_type,
        pixels_per_clock,
        image_sizes
    )

    # speed up with the inner most dimensions (the later ones in the pixels_per_clock array)
    # wrapped the deepest
    for pixel_per_clock in pixels_per_clock[::-1]:
        parallelized_shift_registers = DefineMapParallel(cirb, pixel_per_clock, parallelized_shift_registers)

    return parallelized_shift_registers()

def define_n_dimensional_shift_registers(
        cirb: CoreIRBackend,
        parent_name: str,
        pixel_type: Kind,
        pixels_per_clock: list,
        image_sizes: list) -> Circuit:
    """
    make a high dimensional shift register based on a series of lower dimensional ones
    """
    class _SR(Circuit):

        name = "shift_registers_for_{}_with_{}_pxPerClock_{}_imgSizes".format(
            parent_name,
            cleanName(str(pixels_per_clock)),
            cleanName(str(image_sizes))
        )

        # since may be parallel in each dimension, amount of out ports
        # for each shift register is scaled down by amount of parallelism
        # will be maping over the top shift register for parallelism
        lengths_per_shift_register_per_dimension = [
            image_size // pixel_per_clock for (image_size, pixel_per_clock) in
            zip(image_sizes, pixels_per_clock)
        ]

        IO = ['I', In(pixel_type),
              'O', Out(get_nested_type(pixel_type, lengths_per_shift_register_per_dimension)),
              'next', Out(pixel_type)] + \
             ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            head_pixel_per_clock, *tail_pixels_per_clock = pixels_per_clock
            head_image_size, *tail_image_sizes = image_sizes

            # don't make shift registers, just return SIPO if 1D case
            if len(image_sizes) == 1:
                sipos = SIPOAnyType(cirb, head_image_size // head_pixel_per_clock,
                                    pixel_type, 0, has_ce=True)
                wire(cls.I, sipos.I)
                wire(cls.CE, sipos.CE)
                wire(sipos.O, cls.O)
                wire(sipos.O[-1], cls.next)
                return sipos

            else:
                one_dimensional_lower_shift_register_def = define_n_dimensional_shift_registers(
                    cirb,
                    cls.name,
                    pixel_type,
                    tail_pixels_per_clock,
                    tail_image_sizes)

                one_dimensional_lower_shift_registers = [
                    one_dimensional_lower_shift_register_def() for _ in
                    range(cls.lengths_per_shift_register_per_dimension[0])
                ]

                # connect each next's to the following's input, except the last
                for i in range(len(one_dimensional_lower_shift_registers) - 1):
                    wire(one_dimensional_lower_shift_registers[i].next,
                         one_dimensional_lower_shift_registers[i+1].I)

                # connect the edge input and next of the lower dimenisonal shift registers to those of this
                # higher dimensional shift register
                wire(cls.I, one_dimensional_lower_shift_registers[0].I)
                wire(one_dimensional_lower_shift_registers[-1].next, cls.next)

                for i in range(len(one_dimensional_lower_shift_registers)):
                    wire(one_dimensional_lower_shift_registers[i].O, cls.O[i])
                    wire(cls.CE, one_dimensional_lower_shift_registers[i].CE)

                return one_dimensional_lower_shift_registers

    return _SR


def get_nested_type(pixel_type: Kind, dimensions: list):
    if len(dimensions) == 0:
        return pixel_type
    else:
        return Array(dimensions[0], get_nested_type(pixel_type, dimensions[1:]))