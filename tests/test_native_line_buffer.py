"""Tests for the Aetherling 1D Line Buffer."""

import sys
import random
from itertools import chain
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import GetCoreIRModule

from magma import *
from magma.bitutils import int2seq, seq2int
import coreir
from magma.scope import Scope
from aetherling.modules.native_linebuffer.one_dimensional_native_linebuffer import \
    OneDimensionalLineBuffer, DefineOneDimensionalLineBuffer
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from mantle.common.sipo import SIPO
from mantle.common.countermod import SizedCounterModM
from aetherling.helpers.cli_helper import save_CoreIR_json

def test_basic_native_linebuffer():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ClockInterface(False, False)

    testcircuit = DefineCircuit('create_native_lb_test', *args)

    lb = OneDimensionalLineBuffer(cirb, Array(3, Bit), 1, 3, 100, 1, 0)

    EndCircuit()


def test_multiple_sipo():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ['I', In(Bit), 'O', Out(Array(4, Bit))] + ClockInterface(False, False)

    testcircuit = DefineCircuit('multiple_sipo_test', *args)

    map_sipo = MapParallel(cirb, 1, SIPO(4, 0, has_ce=True))
    wire(1, map_sipo.CE[0])
    wire(testcircuit.I, map_sipo.I[0])
    wire(testcircuit.O, map_sipo.O[0])
    EndCircuit()


    mod = GetCoreIRModule(cirb, testcircuit)
    for p in ["rungenerators", "wireclocks-coreir", "verifyconnectivity-noclkrst",
                             "flattentypes", "flatten", "verifyconnectivity-noclkrst", "deletedeadinstances"]:
        print("Running pass {}".format(p))
        c.run_passes([p], namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    # save_CoreIR_json(cirb, testcircuit, "multiple_sipo.json")
    #sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
    #                      namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])


def test_double_nested_sipo():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ['I', In(Bit), 'O', Out(Array(1,Array(1, Array(4, Bit))))] + ClockInterface(False, False)

    testcircuit = DefineCircuit('multiple_sipo_test', *args)

    map_sipo = MapParallel(cirb, 1, MapParallel(cirb, 1, SIPO(4, 0, has_ce=True)))
    wire(1, map_sipo.CE[0][0])
    wire(testcircuit.I, map_sipo.I[0][0])
    wire(testcircuit.O, map_sipo.O)
    EndCircuit()


    mod = GetCoreIRModule(cirb, testcircuit)
    for p in ["rungenerators", "wireclocks-coreir", "verifyconnectivity-noclkrst",
                             "flattentypes", "flatten", "verifyconnectivity-noclkrst", "deletedeadinstances"]:
        print("Running pass {}".format(p))
        c.run_passes([p], namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    # save_CoreIR_json(cirb, testcircuit, "multiple_sipo.json")
    #sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
    #                      namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])


def test_sipo():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ['I', In(Bit), 'O', Out(Array(4, Bit))] + ClockInterface(False, False)

    testcircuit = DefineCircuit('sipo_test', *args)

    sipo = SIPO(4, 0, has_ce=True)
    wire(1, sipo.CE)
    wire(testcircuit.I, sipo.I)
    wire(testcircuit.O, sipo.O)
    EndCircuit()

    #save_CoreIR_json(cirb, testcircuit, "sipo.json")
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

def test_sized_counter_modm():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ['O', Out(Array(2, Bit))] + ClockInterface(False, False)

    testcircuit = DefineCircuit('sized_counter_modm_test', *args)

    counter = SizedCounterModM(3, has_ce=True)
    wire(1, counter.CE)
    wire(testcircuit.O, counter.O)
    EndCircuit()

    #save_CoreIR_json(cirb, testcircuit, "sized_counter_modm.json")
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

def test_sipo_and_counter():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ['I', In(Bit), 'O_sipo', Out(Array(4, Bit))] + ['O_counter', Out(Array(2, Bit))] + ClockInterface(False, False)

    testcircuit = DefineCircuit('sipo_and_counter_test', *args)

    sipo = SIPO(4, 0, has_ce=True)
    wire(1, sipo.CE)
    wire(testcircuit.I, sipo.I)
    wire(testcircuit.O_sipo, sipo.O)

    counter = SizedCounterModM(3, has_ce=True)
    wire(1, counter.CE)
    wire(testcircuit.O_counter, counter.O)
    EndCircuit()

    #save_CoreIR_json(cirb, testcircuit, "sipo_and_counter.json")
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])


def test_multiple_sipo_and_counter():
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    args = ['I', In(Bit), 'O_sipo', Out(Array(4, Bit))] + ['O_counter', Out(Array(2, Bit))] + ClockInterface(False, False)

    testcircuit = DefineCircuit('multiple_sipo_and_counter_test', *args)

    map_sipo = MapParallel(cirb, 1, SIPO(4, 0, has_ce=True))
    wire(1, map_sipo.CE[0])
    wire(testcircuit.I, map_sipo.I[0])
    wire(testcircuit.O_sipo, map_sipo.O[0])

    counter = SizedCounterModM(3, has_ce=True)
    wire(1, counter.CE)
    wire(testcircuit.O_counter, counter.O)
    EndCircuit()

    #save_CoreIR_json(cirb, testcircuit, "multiple_sipo_and_counter.json")
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

def impl_test_1D_line_buffer(
    magma_type,
    pixels_per_clock: int,
    window_width: int,
    image_size: int,
    output_stride: int,
    origin: int
):
    """Run tests for the 1D line buffer with given token type (magma_type)
and parameters."""

    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()

    # Test line buffer for each combination of parameters and test data.
    for in_arrays in generate_test_data_sets_1D(magma_type, pixels_per_clock, image_size):
        a_1D_line_buffer_test(cirb, scope, in_arrays, magma_type, pixels_per_clock, window_width, image_size, output_stride, origin)

def test_1D_bit_line_buffer_1_3_9_1_0():
    impl_test_1D_line_buffer(Bit, 1, 3, 9, 1, 0)

def test_1D_bit_line_buffer_1_3_9_1_1():
    impl_test_1D_line_buffer(Bit, 1, 3, 9, 1, -1)

def test_1D_bit_line_buffer_2_3_10_1_0():
    impl_test_1D_line_buffer(Bit, 2, 3, 10, 1, 0)

def test_1D_bit_line_buffer_1_3_9_2_0():
    impl_test_1D_line_buffer(Bit, 1, 3, 10, 2, 0)

def test_1D_bit_line_buffer_2_3_10_1_1():
    impl_test_1D_line_buffer(Bit, 2, 3, 10, 1, -1)

def test_1D_bit_line_buffer_1_3_36_1_0():
    impl_test_1D_line_buffer(Bit, 1, 3, 36, 1, 0)

def test_1D_bit_line_buffer_3_3_36_1_0():
    impl_test_1D_line_buffer(Bit, 3, 3, 36, 1, 0)

def test_1D_bit_line_buffer_3_3_36_1_1():
    impl_test_1D_line_buffer(Bit, 3, 3, 36, 1, -1)

def test_1D_bit_line_buffer_1_4_32_2_1():
    impl_test_1D_line_buffer(Bit, 1, 4, 32, 2, -1)

def test_1D_bit_line_buffer_4_8_32_2_1():
    impl_test_1D_line_buffer(Bit, 4, 8, 32, 2, -1)

def test_1D_bit_line_buffer_1_3_26_2_0():
    impl_test_1D_line_buffer(Bit, 1, 3, 26, 2, 0)

def test_1D_bit_line_buffer_1_3_30_1_1():
    impl_test_1D_line_buffer(Bit, 1, 3, 30, 1, -1)

def test_1D_bit_line_buffer_16_4_64_2_1():
    impl_test_1D_line_buffer(Bit, 16, 4, 64, 2, -1)

def test_1D_bit_line_buffer_1_2_18_2_0():
    impl_test_1D_line_buffer(Bit, 1, 2, 18, 2, 0)

def test_1D_bit_line_buffer_4_8_24_4_2():
    impl_test_1D_line_buffer(Bit, 4, 8, 24, 4, -2)

def test_1D_bit_line_buffer_4_8_20_2_3():
    impl_test_1D_line_buffer(Bit, 4, 8, 20, 2, -3)

def test_1D_bit_line_buffer_13_5_39_1_0():
    impl_test_1D_line_buffer(Bit, 13, 5, 39, 1, 0)

def test_1D_bit_line_buffer_1_15_39_13_1():
    impl_test_1D_line_buffer(Bit, 1, 15, 39, 13, -1)

def test_1D_bit_line_buffer_2_3_14_2_1():
    impl_test_1D_line_buffer(Bit, 2, 3, 14, 2, -1)


def test_1D_int4_line_buffer_1_3_9_1_0():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 3, 9, 1, 0)

def test_1D_int4_line_buffer_1_3_9_1_1():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 3, 9, 1, -1)

def test_1D_int4_line_buffer_2_3_10_1_0():
    impl_test_1D_line_buffer(Array(4, Bit), 2, 3, 10, 1, 0)

def test_1D_int4_line_buffer_1_3_9_2_0():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 3, 10, 2, 0)

def test_1D_int4_line_buffer_2_3_10_1_1():
    impl_test_1D_line_buffer(Array(4, Bit), 2, 3, 10, 1, -1)

def test_1D_int4_line_buffer_1_3_36_1_0():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 3, 36, 1, 0)

def test_1D_int4_line_buffer_3_3_36_1_0():
    impl_test_1D_line_buffer(Array(4, Bit), 3, 3, 36, 1, 0)

def test_1D_int4_line_buffer_3_3_36_1_1():
    impl_test_1D_line_buffer(Array(4, Bit), 3, 3, 36, 1, -1)

def test_1D_int4_line_buffer_1_4_32_2_1():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 4, 32, 2, -1)

def test_1D_int4_line_buffer_4_8_32_2_1():
    impl_test_1D_line_buffer(Array(4, Bit), 4, 8, 32, 2, -1)

def test_1D_int4_line_buffer_1_3_26_2_0():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 3, 26, 2, 0)

def test_1D_int4_line_buffer_1_3_30_1_1():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 3, 30, 1, -1)

def test_1D_int4_line_buffer_16_4_64_2_1():
    impl_test_1D_line_buffer(Array(4, Bit), 16, 4, 64, 2, -1)

def test_1D_int4_line_buffer_1_2_18_2_0():
    impl_test_1D_line_buffer(Array(4, Bit), 1, 2, 18, 2, 0)

def test_1D_int4_line_buffer_4_8_24_4_2():
    impl_test_1D_line_buffer(Array(4, Bit), 4, 8, 24, 4, -2)

def test_1D_int4_line_buffer_4_8_20_2_3():
    impl_test_1D_line_buffer(Array(4, Bit), 4, 8, 20, 2, -3)

def test_1D_int8_line_buffer_13_5_39_1_0():
    impl_test_1D_line_buffer(Array(8, Bit), 13, 5, 39, 1, 0)

def test_1D_int8_line_buffer_1_15_39_13_1():
    impl_test_1D_line_buffer(Array(8, Bit), 1, 15, 39, 13, -1)

def test_1D_int8_line_buffer_2_3_14_2_1():
    impl_test_1D_line_buffer(Array(8, Bit), 2, 3, 14, 2, -1)


def a_1D_line_buffer_test(
    cirb,
    scope,
    in_arrays,
    magma_type,
    pixels_per_clock: int,
    window_width: int,
    image_size: int,
    output_stride: int,
    origin: int
):
    """Simulate a line buffer with the given parameters, token type, and
    specified input (list-of-lists, outer list is time and inner list
    represents array of values). Use python int type to represent int;
    we translate to array of bits at the last second.
    """
    window_count, parallelism, valid_count = internal_params_1D(
        pixels_per_clock, window_width, image_size, output_stride, origin
    )

    expected = expected_valid_outputs_1D(
        in_arrays,
        pixels_per_clock, window_width, image_size, output_stride, origin
    )

    LineBufferDef = DefineOneDimensionalLineBuffer(
        cirb, magma_type, pixels_per_clock, window_width, image_size, output_stride, origin
    )

    #save_CoreIR_json(cirb, LineBufferDef, "native_linebuffer.json")

    sim = CoreIRSimulator(LineBufferDef, LineBufferDef.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(LineBufferDef.CE, 1, scope)

    # List for recording sequence of valid outputs (in the same format
    # as specified by expected_valid_outputs_1D), and a two helper
    # functions. tick_sim_collect_outputs ticks the simulation,
    # appending any output received when the simulated module asserts
    # valid. set_value feeds a new array input to the
    # simulator. There's one version for bit and one for int (annoying
    # leaky abstraction).
    actual = []
    if magma_type == Bit:
        def tick_sim_collect_outputs():
            sim.evaluate()
            sim.advance_cycle()
            if sim.get_value(LineBufferDef.valid, scope):
                actual.append(
                    [
                        [
                            bool(sim.get_value(LineBufferDef.O[par][pix], scope))
                            for pix in range(window_width)
                        ]
                        for par in range(parallelism)
                    ]
                )
        def set_value(bit_array):
            sim.set_value(LineBufferDef.I, bit_array, scope)
    else:
        try:
            bit_width = magma_type.N
            if magma_type.T != Bit:
                raise TypeError("Array not made of Bits.")
        except Exception as e:
            raise TypeError("magma_type appears not to be Bit "
                "or Array of Bit.") from e

        # So, we need to pad the sequence of bits with zero because if it
        # doesn't match the expected bit width exactly, we will segfault.
        def padded_int2seq(x):
            return int2seq(x, n=bit_width)

        def tick_sim_collect_outputs():
            sim.evaluate()
            sim.advance_cycle()
            if sim.get_value(LineBufferDef.valid, scope):
                actual.append(
                    [
                        [
                            seq2int(sim.get_value(LineBufferDef.O[par][pix], scope))
                            for pix in range(window_width)
                        ]
                        for par in range(parallelism)
                    ]
                )
        def set_value(int_array):
            sim.set_value(LineBufferDef.I,
                list(map(padded_int2seq, int_array)), scope)

    for array in in_arrays:
        set_value(array)
        tick_sim_collect_outputs()

    # Wait for a little extra after the last input because the line buffer
    # may need a bit of extra time to emit the last windows (also, it gives
    # us a chance to check for excessive valid outputs).
    extra_cycles = 16 + window_width
    for i in range(extra_cycles):
        if len(actual) >= len(expected): break
        tick_sim_collect_outputs()

    len_actual, len_expected = len(actual), len(expected)

    assert len_actual >= len_expected, \
        f"Got {len_actual} outputs (counts of valid asserts); expected " \
        f"{len_expected} outputs.\n" \
        f"Waited {extra_cycles} cycles after last input."

    assert outputs_match_1D(actual[0:len_expected], expected), \
        "Outputs don't match."

def expected_valid_outputs_1D(
    in_arrays,
    pixels_per_clock: int,
    window_width: int,
    image_size: int,
    output_stride: int,
    origin: int
):
    """Given 1D line buffer parameters and a list of lists of pixel
    values representing the stream of input (one entry = one clock
    cycles' array-of-pixels input), return a list-of-lists-of-lists of
    values (None for garbage) representing outputs on cycles where the
    linebuffer asserts valid, in the order:
        outer dimension :  time
        middle dimension:  list of windows (parallelism)
        inner dimension :  pixels within a window
    """
    stride = output_stride
    if len(in_arrays) * pixels_per_clock != image_size:
        raise Exception(
            "Expected in_arrays length of %g"
                 % (image_size/pixels_per_clock)
            + f" for pixels_per_clock {pixels_per_clock}"
            + f" and image_size {image_size}."
        )
    # index -> pixel
    pixel_dict = {i:b for i,b in enumerate(chain(*in_arrays))}

    window_count, parallelism, valid_count = internal_params_1D(
        pixels_per_clock, window_width, image_size, output_stride, origin
    )

    expected= \
    [
        [
            [
                pixel_dict.get(window_offset + pixel_offset)
                for pixel_offset in range(window_width)
            ]
            for window_offset # index of left pixel of window.
            in range(origin + parallelism * stride * time_idx,
                     origin + parallelism * stride * (1+time_idx),
                     stride)
        ]
        for time_idx in range(valid_count)
    ]
    return expected

def internal_params_1D(
    pixels_per_clock: int,
    window_width: int,
    image_size: int,
    output_stride: int,
    origin: int
):
    """Calculate "internal" parameters of linebuffer based on user parameters.

    This includes the window_count (number of total windows outputted),
    the parallelism (width of the output bus in number of windows),
    and the valid_count (number of times valid should be asserted).

    Return as tuple (window_count, parallelism, valid_count).
    """
    stride = output_stride

    # Total number of windows outputted.
    window_count = image_size//stride

    # Number of parallel window outputs.
    parallelism = pixels_per_clock//stride
    if parallelism == 0:
        parallelism = 1
    else:
        assert parallelism*stride == pixels_per_clock, \
            "Expected integer throughput (stride evenly dividing px/clk)."

    # Number of times valid should be asserted.
    valid_count = window_count//parallelism
    assert valid_count * parallelism == window_count, \
        "Expected window count (img/stride) to be divisible by parallelism " \
        "(px/clk / stride)"

    return window_count, parallelism, valid_count

def outputs_match_1D(actual, expected):
    """Compare two 3D-arrays (in format as in expected_valid_output) and
see if they match. The reason we need this function instead of just ==
is that the module is allowed to output anything when the expected
value is None (i.e. we expect garbage)."""
    stderr = lambda *args: print(*args, file=sys.stderr)

    def windows_match(tup):
        actual, expected = tup
        if len(actual) != len(expected):
            stderr("Window sizes don't match")
            return False
        return all(map(
            lambda tup: tup[1] == None or tup[0] == tup[1],
            zip(actual, expected)
        ))

    if len(actual) != len(expected):
        stderr("Different number of valid outputs.")
        return False
    else:
        for (actual_par, expected_par) in zip(actual, expected):
            if len(actual_par) != len(expected_par):
                stderr("Different parallelism (i.e. output bus width.)")
                return False
            else:
                return all(map(windows_match, zip(actual_par, expected_par)))

def generate_test_data_sets_1D(
    magma_type,
    pixels_per_clock: int,
    image_size: int
):
    """Make a 3D-list of test data suitable for testing a 1D line buffer
with the given pixels/clock, image size parameters, and the given
token type (as magma Bit or Array of Bit), Outer dim = list of
separate test sets, middle dim = input over clock cycles, inner dim =
array entries.
    """
    if magma_type == Bit:
        return generate_test_data_sets_1D_bits(pixels_per_clock, image_size)
    else:
        try:
            n = magma_type.N
            t = magma_type.T
            if t != Bit: raise TypeError("Not a bit.")
            return generate_test_data_sets_1D_ints(
                pixels_per_clock, image_size, n
            )
        except Exception as e:
            raise TypeError("magma_type was not Bit and failed at "
                "interpreting as array of bits") from e

def generate_test_data_sets_1D_bits(
    pixels_per_clock: int,
    image_size: int,
):
    """Make a 3D-list of test data suitable for testing a 1D bit line
buffer with the given pixels/clock and image size parameters. Outer
dim = list of separate test sets, middle dim = input over clock
cycles, inner dim = array entries.
    """

    # Make some random bit generators and some simple predictable generators.
    # Repating patterns first.
    alternating_value = True
    def alternating():
        nonlocal alternating_value
        alternating_value = not alternating_value
        return alternating_value

    value_third = -1
    def third_true():
        nonlocal value_third
        value_third += 1
        return value_third == 3

    value_fifth = -1
    # note: false comes first
    def every_fifth_false():
        nonlocal value_fifth
        value_fifth += 1
        return value_fifth % 5 != 0

    bit_generators = [alternating, third_true, every_fifth_false]

    # Now add pseudorandom patterns.
    bit_generators += [
        lambda: rng.random() >= 0.5 for rng in
        [random.Random(seed) for seed in [2001, 1, 6]] # <3
    ]

    # For each generator create a test set.
    return [
        generate_one_test_data_set_1D(
            generator,
            pixels_per_clock,
            image_size
        )
        for generator in bit_generators
    ]

def generate_test_data_sets_1D_ints(
    pixels_per_clock: int,
    image_size: int,
    bit_width: int,
):
    """Make a 3D-list of test data suitable for testing a 1D int line
buffer with the given pixels/clock, image size, and integer bit-width
parameters. Outer dim = list of separate test sets, middle dim = input
over clock cycles, inner dim = array entries.
    """
    # Mask to ensure generated int fits in within our specified bit width.
    mask = (1 << bit_width) - 1

    # First, two predictable generators.
    sawtooth_value = -1
    def sawtooth():
        nonlocal sawtooth_value
        sawtooth_value += 1
        return sawtooth_value & mask

    sawtooth19_value = -19
    def sawtooth19():
        nonlocal sawtooth19_value
        sawtooth19_value += 19
        return sawtooth19_value & mask

    int_generators = [sawtooth, sawtooth19]

    # Now add pseudorandom patterns.
    # Note: I don't think randrange is guaranteed to give the same
    # result across versions so I'm manually mapping float->int.
    int_generators += [
        lambda: int(rng.random() * 0x80000000) & mask for rng in
        [random.Random(seed) for seed in [1998, 7, 24]]
    ]

    # For each generator create a test set.
    return [
        generate_one_test_data_set_1D(
            generator,
            pixels_per_clock,
            image_size
        )
        for generator in int_generators
    ]

def generate_one_test_data_set_1D(
    random_generator,
    pixels_per_clock: int,
    image_size: int,
):
    """Generate a 2D-list of test data suitable for testing a 1D line
buffer with the given pixels/clock and image size parameters. Populate
the test data using values from the random_generator function passed.
    """

    cycle_count = image_size//pixels_per_clock
    assert cycle_count * pixels_per_clock == image_size, \
        "Expected pixels/clock to evenly divide pixel count."

    return [
        [random_generator() for i in range(pixels_per_clock)]
        for s in range(cycle_count)
    ]
