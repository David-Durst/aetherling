from aetherling.helpers.nameCleanup import cleanName
from magma import *
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from aetherling.modules.map_fully_parallel_sequential import DefineMapParallel, DefineNativeMapParallel
from aetherling.modules.sipo_any_type import SIPOAnyType
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.delayed_buffer import DelayedBuffer
from math import ceil
from itertools import product
import builtins
from functools import reduce
from operator import mul


def get_nested_type(pixel_type: Kind, dimensions: list):
    if len(dimensions) == 0:
        return pixel_type
    else:
        return Array[dimensions[0], get_nested_type(pixel_type, dimensions[1:])]

def DefineAnyDimensionalLineBuffer(
        pixel_type: Kind,
        pixels_per_clock: list,
        window_widths: list,
        image_sizes: list,
        strides: list,
        origins: list) -> Circuit:

    class _LB(Circuit):
        if len(pixels_per_clock) != len(window_widths) and \
            len(window_widths) != len(image_sizes) and \
            len(image_sizes) != len(strides) and \
            len(strides) != len(origins):
            raise Exception("not all inputs same dimensionality for any dimension linebuffer")

        if len(image_sizes) == 0:
            raise Exception("any dimensional linebuffer must have at least 1D inputs, parameters are empty lists")

        name_args = [cleanName(str(s)) for s in [pixel_type, pixels_per_clock, window_widths, image_sizes,
            strides, [abs(i) for i in origins]]]
        name = "AnyDimensionalLineBuffer_{}type_{}pxPerClock_{}window" \
               "_{}img_{}stride_{}origin".format(*name_args)
        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        windows_per_active_clock = [max(p // s, 1) for (p, s) in zip(pixels_per_clock, strides)]

        IO = ['I', In(get_nested_type(pixel_type, pixels_per_clock)),
              'O', Out(get_nested_type(pixel_type, windows_per_active_clock + window_widths))] + \
             ['valid', Out(Bit)] + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):

            num_dimensions = len(pixels_per_clock)

            # this variable provides a location in 2N dimensions, where N is the number
            # of dimensions in the images. The dimensions using shift registers is
            # 2N dimensional as you need an extra dimension for each level of parallelism.
            # greater values in dimensions are earlier indexes in input image
            # the lower index half of dimensions are for picking which parallel shift register
            # the higher index half of dimensions are for which entry in a shift register
            # for example if N is 4, entry 3 is for picking the parallel shift registers for the 4th dimension,
            # and entry 7 is for picking the entry for that shift register
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

                # get the maximum coordinate when aligning needed pixels to the number
                # of pixels per clock

                # if need more than all values in a dimension (as parallelism is so great that
                # a window extends off the last value in this dimension) then oldest_need is just
                # the last valid entry in this dimension
                if needed_pixels_cur_dim >= image_sizes[dimension]:
                    oldest_needed_pixel_forward_ND_coordinates[dimension] = image_sizes[dimension]
                elif needed_pixels_cur_dim % pixels_per_clock[dimension] == 0:
                    oldest_needed_pixel_forward_ND_coordinates[dimension] = needed_pixels_cur_dim
                else:
                    oldest_needed_pixel_forward_ND_coordinates[dimension] = \
                        ceil(needed_pixels_cur_dim / pixels_per_clock[dimension]) * pixels_per_clock[dimension]

                # adjust by 1 for 0 indexing
                oldest_needed_pixel_forward_ND_coordinates[dimension] -= 1

            def set_shift_register_location_using_ND_coordinates(new_coordinates):
                # carry ensures if go over edge in one dimension, can just move to next dimension
                interdimension_carry = 0
                for cur_dimension in range(num_dimensions)[::-1]:
                    # max is to verify that stride doesn't force last windows past beginning of shift
                    # registers
                    ND_coordinate_reversed_indexing = max(
                        oldest_needed_pixel_forward_ND_coordinates[cur_dimension] + interdimension_carry -
                        new_coordinates[cur_dimension], 0
                    )

                    interdimension_carry = ND_coordinate_reversed_indexing // image_sizes[cur_dimension]
                    ND_coordinate_reversed_indexing %= image_sizes[cur_dimension]

                    coordinates_2N_dimensional[num_dimensions + cur_dimension] = \
                        ND_coordinate_reversed_indexing // pixels_per_clock[cur_dimension]
                    coordinates_2N_dimensional[cur_dimension] = \
                        ND_coordinate_reversed_indexing % pixels_per_clock[cur_dimension]


            # used coordinates tracks all the ND coordinates used for window outputs
            used_coordinates = set()

            # output indicies and what outputs of shift registers to wire to them
            output_to_shift_register_mapping = set()

            window_indices = product(*[range(w) for w in cls.windows_per_active_clock])


            for current_window_index in window_indices:
                # stride is handled by wiring if there are multiple windows emitted per clock,
                # aka if stride is less than number of pixels per clock.
                # In this case, multiple windows are emitted but they must be overlapped
                # less than normal
                window_coordinate = []
                for cur_dimension in range(num_dimensions):
                    strideMultiplier = strides[cur_dimension]
                    window_coordinate.append(
                        strideMultiplier * current_window_index[cur_dimension] +
                        # handle origin across multiple clocks by changing valid, but within a single clock
                        # need to adjust where the windows start
                        # need neg conversion twice due to issues taking mod of negative number
                        ((origins[cur_dimension] * -1) % pixels_per_clock[cur_dimension] * -1)
                    )
                # for every value in window, wire up output to location in shift registers
                for coordinates_in_window in product(*[range(w) for w in window_widths]):
                    set_shift_register_location_using_ND_coordinates([sum(window_and_coordinates_in_it) for
                                          window_and_coordinates_in_it in
                                         zip(window_coordinate, coordinates_in_window)])


                    used_coordinates.add(builtins.tuple(coordinates_2N_dimensional))

                    output_to_shift_register_mapping.add((
                        current_window_index + coordinates_in_window,
                        builtins.tuple(coordinates_2N_dimensional)
                    ))

            # 2 times num_dimenions as those are ND dimensions and this is for 2ND dimensions
            needed_2ND_coordinates = get_need_size_for_each_dimension(used_coordinates, 2 * num_dimensions)
            shift_registers = create_parallel_shift_registers(cls.name, pixel_type,
                                            pixels_per_clock, image_sizes, needed_2ND_coordinates)


            def recursively_wire_input(lb_inputs, shift_register_inputs,
                                       lb_CE, shift_register_CE, dimension_depth):
                # reverse the pixels per clock. Since greater index_in_shift_register
                # mean earlier inputted pixels, also want greater current_shift_register
                # to mean earlier inputted pixels. This accomplishes that by making
                # pixels earlier each clock go to higher number shift register
                if dimension_depth == 1:
                    wire(lb_inputs[::-1], shift_register_inputs)
                    for i in range(len(shift_register_CE)):
                        wire(lb_CE, shift_register_CE[i])
                else:
                    for i in range(len(lb_inputs)):
                        recursively_wire_input(lb_inputs[len(lb_inputs) - i - 1],
                                               shift_register_inputs[i], lb_CE,
                                               shift_register_CE[i], dimension_depth - 1)

            recursively_wire_input(cls.I, shift_registers.I,
                                   cls.CE, shift_registers.CE, len(pixels_per_clock))

            def multidimensionalLookup(arr, indexes):
                if len(indexes) == 1:
                    return arr[indexes[0]]
                else:
                    return multidimensionalLookup(arr[indexes[0]], indexes[1:])

            for mapping in output_to_shift_register_mapping:
                (output_index, shift_register_index) = mapping
                # print("wiring sr {} to output {}".format(
                #     multidimensionalLookup(shift_registers.O, shift_register_index),
                #     multidimensionalLookup(cls.O, output_index)
                # ))
                wire(multidimensionalLookup(shift_registers.O, shift_register_index),
                     multidimensionalLookup(cls.O, output_index))

            possibly_used_coordinates = product(*[range(w) for w in needed_2ND_coordinates])

            # wire up all non-used coordinates to terms
            for c in possibly_used_coordinates:
                if c in used_coordinates:
                    continue
                term = TermAnyType(pixel_type)
                wire(multidimensionalLookup(shift_registers.O, c), term.I)

            # will be valid when go through all but last needed for each dimension

            # this variable tracks how many clocks it takes to complete this dimension
            # start with 1 as the time to complete each pixel is 1 clock, not really a dimension
            # but helpful to have this for computing other dimensions
            clocks_to_complete_each_dimension = [1]

            for d in range(num_dimensions)[::-1]:
                clocks_to_complete_each_dimension = [image_sizes[d] // pixels_per_clock[d] *
                                                     clocks_to_complete_each_dimension[0]] + clocks_to_complete_each_dimension


            # to compute valid_counter_max_value (aka when to be valid after warmup):
            # for each dimension other than inner most, need to fill in 1 less than oldest needed.
            # then, for inner most, fill that up thrgouh oldest needed. Then ready to start emitting values
            clocks_to_fill_in_outer_dimensions = 0
            for d in range(num_dimensions)[:-1]:
                # the time to complete dimension d+1 is the time to get 1 value for dimension d
                clocks_to_fill_in_outer_dimensions += clocks_to_complete_each_dimension[d+1] * (
                    # subtract 1 here as need second to oldest needed pixel in this dimension
                    # but then add 1 as 0 indexed, so multiply by 3 for pixel 2.
                    # these cancel each other out
                    (oldest_needed_pixel_forward_ND_coordinates[d] + origins[d]) // pixels_per_clock[d]
                )


            # valid when the maximum coordinate used in the inner most dimension and all outer most dimensions
            # have been satisfied
            # add 1 as reading from a register, 1 cycle delay
            valid_counter_max_value = ceil((oldest_needed_pixel_forward_ND_coordinates[-1] + 1 + origins[-1]) /
                                           pixels_per_clock[-1]) + clocks_to_fill_in_outer_dimensions

            # add 1 as sizedcounter counts to 1 less than the provided max
            valid_counter = SizedCounterModM(valid_counter_max_value + 1, has_ce=True)

            valid_counter_max_instance = DefineCoreirConst(len(valid_counter.O),
                                                           valid_counter_max_value)()

            wire(enable(bit(cls.CE) &
                        (valid_counter.O < valid_counter_max_instance.O)), valid_counter.CE)

            # for each dimnesion, if stride is greater than pixels_per_clock, then need a stride counter as
            # not active every clock. Invalid clocks create striding in this case
            trueGen = DefineCoreirConst(1,1)()
            stride_counters_valid = trueGen.O == trueGen.O

            for d in range(num_dimensions):
                if strides[d] > pixels_per_clock[d]:

                    stride_counter = SizedCounterModM(strides[d] // pixels_per_clock[d]
                                                      * clocks_to_complete_each_dimension[d + 1], has_ce=True)
                    stride_counter_valid_values = DefineCoreirConst(len(stride_counter.O),
                                                                    clocks_to_complete_each_dimension[d + 1])()

                    stride_counters_valid = stride_counters_valid & (
                            stride_counter.O < stride_counter_valid_values.O
                    )

                    # only increment stride if trying to emit data this clock cycle
                    wire((valid_counter.O == valid_counter_max_instance.O) & bit(cls.CE), stride_counter.CE)

            wire(enable(stride_counters_valid &
                        (valid_counter.O == valid_counter_max_instance.O)),
                 cls.valid)

    return _LB


def AnyDimensionalLineBuffer(
        pixel_type: Kind,
        pixel_per_clock: list,
        window_width: list,
        image_size: list,
        stride: list,
        origin: list) -> Circuit:

    return DefineAnyDimensionalLineBuffer(
        pixel_type,
        pixel_per_clock,
        window_width,
        image_size,
        stride,
        origin
    )()

def get_need_size_for_each_dimension(used_coordinates: list, num_dimensions) -> list:
    """
    Given a list of tuples, where each tuple is a 2N dimensional coordinate, get back a tuple of length 2N with the
    max value in each coordinate (plus 1 so the number is correct for sizing buffers)
    :param used_coordinates: The list of 2N coordinats
    :return: A 2N length list of max value in each dimension
    """
    max_coordinates = [0] * num_dimensions
    for coordinate in used_coordinates:
        for dimension in range(num_dimensions):
            max_coordinates[dimension] = max(max_coordinates[dimension], coordinate[dimension] + 1)
    return max_coordinates

def create_parallel_shift_registers(
        parent_name: str,
        pixel_type: Kind,
        pixels_per_clock: list,
        image_sizes: list,
        needed_2ND_coordinates: list) -> Circuit:
    """
    Create the nested parallel, high dimensional shift registers
    """
    num_dimensions = len(image_sizes)


    parallelized_shift_registers_list = [define_n_dimensional_shift_registers(
        parent_name,
        pixel_type,
        pixels_per_clock,
        image_sizes,
        needed_2ND_coordinates[num_dimensions:],
        True
    )]

    # speed up with the inner most dimensions (the later ones in the pixels_per_clock array)
    # wrapped the deepest
    for pixel_per_clock in pixels_per_clock[::-1]:
        parallelized_shift_registers_list.append(
            DefineNativeMapParallel(pixel_per_clock, parallelized_shift_registers_list[-1])
        )

    parallelized_shift_registers = parallelized_shift_registers_list[-1]()

    return parallelized_shift_registers

def define_n_dimensional_shift_registers(
        parent_name: str,
        pixel_type: Kind,
        pixels_per_clock: list,
        image_sizes: list,
        needed_2ND_coordinates: list,
        last_in_dimension: bool) -> Circuit:
    """
    make a high dimensional shift register based on a series of lower dimensional ones
    Note: last_in_dimension if the shift register is the last is its dimension
    """
    class _SR(Circuit):

        name = "shift_registers_for_{}_with_{}_pxPerClock_{}_imgSizes_{}_neededCoordinates_{}_lastInDimension".format(
            parent_name,
            cleanName(str(pixels_per_clock)),
            cleanName(str(image_sizes)),
            cleanName(str(needed_2ND_coordinates)),
            str(last_in_dimension)
        )

        # since may be parallel in each dimension, amount of out ports
        # for each shift register is scaled down by amount of parallelism
        # will be maping over the top shift register for parallelism
        lengths_per_shift_register_per_dimension = [
            image_size // pixel_per_clock for (image_size, pixel_per_clock) in
            zip(image_sizes, pixels_per_clock)
        ]

        second_to_last_2ND_coordinate = [needed_2ND_coordinates[-2] - 1] if len(needed_2ND_coordinates) > 1 else []

        IO = ['I', In(pixel_type),
              'O', Out(get_nested_type(pixel_type, needed_2ND_coordinates))] + \
             (['next', Out(pixel_type)] if not last_in_dimension else []) + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            head_pixel_per_clock, *tail_pixels_per_clock = pixels_per_clock
            head_image_size, *tail_image_sizes = image_sizes
            head_needed_2ND_coordinates, *tail_needed_2ND_coordinates = needed_2ND_coordinates

            # don't make shift registers, just return SIPO if 1D case
            if len(image_sizes) == 1:
                # if need outputs from all or all but last element in this shift register, just use registers
                # otherwise use an SRAM
                rest_of_row_buffer_size = cls.lengths_per_shift_register_per_dimension[0] - head_needed_2ND_coordinates
                if rest_of_row_buffer_size == 1 and not last_in_dimension:
                    sipos = SIPOAnyType(head_needed_2ND_coordinates + 1, pixel_type, 0, has_ce=True)
                else:
                    sipos = SIPOAnyType(head_needed_2ND_coordinates, pixel_type, 0, has_ce=True)
                wire(cls.I, sipos.I)
                wire(cls.CE, sipos.CE)
                wire(sipos.O[0:head_needed_2ND_coordinates], cls.O)
                if not last_in_dimension:
                    # feed one element every clock, delayed on output by size of buffer so equivalent to shift register
                    if rest_of_row_buffer_size < 2:
                        wire(sipos.O[-1], cls.next)
                    else:
                        rowbuffer = DelayedBuffer(pixel_type, rest_of_row_buffer_size, 1,
                                                  rest_of_row_buffer_size, rest_of_row_buffer_size)
                        wire(sipos.O[-1], rowbuffer.I[0])
                        wire(rowbuffer.O[0], cls.next)
                        valid_term = TermAnyType(Bit)
                        wire(rowbuffer.valid, valid_term.I)
                        wire(cls.CE, rowbuffer.CE)
                        wire(cls.CE, rowbuffer.WE)

                return sipos

            else:

                # if this shift register is the last in its dimension, make the last of its children also a last
                # and only make enough of them to fill the output ports. Otherwise, make enough to fill this
                # dimension
                one_dimensional_lower_shift_register_def_not_last_in_dimension = define_n_dimensional_shift_registers(
                    cls.name,
                    pixel_type,
                    tail_pixels_per_clock,
                    tail_image_sizes,
                    tail_needed_2ND_coordinates,
                    False
                )

                one_dimensional_lower_shift_register_def_last_in_dimension = define_n_dimensional_shift_registers(
                    cls.name,
                    pixel_type,
                    tail_pixels_per_clock,
                    tail_image_sizes,
                    tail_needed_2ND_coordinates,
                    True
                )

                if last_in_dimension:
                    one_dimensional_lower_shift_registers = [
                        one_dimensional_lower_shift_register_def_not_last_in_dimension() for _ in
                        range(head_needed_2ND_coordinates - 1)
                    ]

                    one_dimensional_lower_shift_registers += [
                        one_dimensional_lower_shift_register_def_last_in_dimension()
                    ]
                else:
                    one_dimensional_lower_shift_registers = [
                        one_dimensional_lower_shift_register_def_not_last_in_dimension() for _ in
                        range(cls.lengths_per_shift_register_per_dimension[0])
                    ]

                # connect each next's to the following's input, except the last
                for i in range(len(one_dimensional_lower_shift_registers) - 1):
                    wire(one_dimensional_lower_shift_registers[i].next,
                         one_dimensional_lower_shift_registers[i+1].I)

                # connect the edge input and next of the lower dimenisonal shift registers to those of this
                # higher dimensional shift register
                wire(cls.I, one_dimensional_lower_shift_registers[0].I)

                if not last_in_dimension:
                    wire(one_dimensional_lower_shift_registers[-1].next, cls.next)

                for i in range(len(one_dimensional_lower_shift_registers)):
                    wire(one_dimensional_lower_shift_registers[i].O, cls.O[i])
                    wire(cls.CE, one_dimensional_lower_shift_registers[i].CE)


                return one_dimensional_lower_shift_registers

    return _SR