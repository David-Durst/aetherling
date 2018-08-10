"""Tests for the Aetherling 2D Line Buffer.

Note: The simulated data is passed around as a 4D list for output, 3D for input.
    Outer dimension: Clock cycles
    Middle dimension: "Parallelism", i.e. multiple windows emitted per cycle
        (this dimension is excluded for the input).
    Inner 2 dimensions: list of rows of pixels in 1 window.

    Example: [4][1][2][3] means the pixel at row 2, col 3 of the output window that
    was emitted on clock cycle 4 through output port 1."""

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
from aetherling.modules.native_linebuffer.two_dimensional_native_linebuffer import TwoDimensionalLineBuffer, \
    DefineTwoDimensionalLineBuffer
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from mantle.common.sipo import SIPO
from mantle.common.countermod import SizedCounterModM
from aetherling.helpers.cli_helper import save_CoreIR_json

class LineBufferParameters(object):
    """Bundle of data for 2D line buffer parameters."""
    __slots__ = [
        "magma_type",
        "y_per_clk", "x_per_clk",
        "window_y", "window_x",
        "image_y", "image_x",
        "stride_y", "stride_x",
        "origin_y", "origin_x",
    ]

    def __init__(
        self,
        magma_type,
        y_per_clk=1, x_per_clk=1,
        window_y=1, window_x=1,
        image_y=1, image_x=1,
        stride_y=1, stride_x=1,
        origin_y=0, origin_x=0
    ):
        self.magma_type = magma_type
        self.y_per_clk = y_per_clk
        self.x_per_clk = x_per_clk
        self.window_y = window_y
        self.window_x = window_x
        self.image_y = image_y
        self.image_x = image_x
        self.stride_y = stride_y
        self.stride_x = stride_x
        self.origin_y = origin_y
        self.origin_x = origin_x

        self.validate()

    def validate(self):
        """Raise ValueError in case the parameters don't satisfy line buffer
requirements (especially divisibility requirements).
        """
        if any (arg <= 0 for arg in [self.y_per_clk, self.x_per_clk,
                                     self.window_y, self.window_x,
                                     self.image_y, self.image_x,
                                     self.stride_y, self.stride_x]):
            raise ValueError("Can't have nonpositive line buffer parameter\n"
                + str(self.as_kwargs()))

        if self.y_per_clk != 1:
            raise ValueError("Need y_per_clk (px/clk height) to be 1.")

        a_divides_b = lambda a, b: b % a == 0

        if not all(a_divides_b for (a, b) in [(self.y_per_clk, self.image_y),
                                              (self.x_per_clk, self.image_x),
                                              (self.stride_y, self.image_y),
                                              (self.stride_x, self.image_x)]):
            raise ValueError("Need input px/clk width (height) and\n"
                "stride width (height) to divide image width (height) for\n"
                + str(self.as_kwargs()))

        window_throughput = (self.x_per_clk * self.y_per_clk /
                             (self.stride_x * self.stride_y))

        stride_area = self.stride_y * self.stride_x
        px_per_clk_area = self.y_per_clk * self.x_per_clk

        if (not a_divides_b(stride_area, px_per_clk_area) and
            not a_divides_b(px_per_clk_area, stride_area)):
            # i.e. window throughput needs to be int or reciprocal of int.
            raise ValueError("Need integer or integer-reciprocal window "
                f"throughput.\nWindow throughput={window_throughput} in\n"+
                str(self.as_kwargs()))

        if (self.origin_y > 0 or self.origin_y <= -self.window_y or
            self.origin_x > 0 or self.origin_x <= -self.window_x):

            raise ValueError("Origin must be between (0, 0) and "
                "(-window_y+1, -window_x+1)\n" + str(self.as_kwargs()))

    def as_kwargs(self):
        """Return parameters held within as a dict suitable for passing as
kwargs (using **) to TwoDimensionalLineBuffer.
        """
        return {
            'pixel_type':self.magma_type,
            'pixels_per_row_per_clock':self.x_per_clk,
            'rows_of_pixels_per_clock':self.y_per_clk,
            'window_cols':self.window_x,
            'window_rows':self.window_y,
            'image_cols':self.image_x,
            'image_rows':self.image_y,
            'stride_cols':self.stride_x,
            'stride_rows':self.stride_y,
            'origin_cols':self.origin_x,
            'origin_rows':self.origin_y,
        }

    def internal_parameters(self):
        """Calculate "internal" parameters of line buffer based on the
        parameters specified in this object. Return as tuple of

        window_count: total number of windows emitted,
        parallelism: width of output bus in number of windows,
        valid_count: number of times valid should be asserted.
        """
        stride_area = self.stride_y * self.stride_x
        px_per_clk_area = self.y_per_clk * self.x_per_clk

        window_count =  self.image_y * self.image_x // stride_area
        parallelism = max(1, px_per_clk_area // stride_area)
        valid_count = window_count // parallelism

        return (window_count, parallelism, valid_count)

def expected_valid_outputs_2D(
    in_arrays,
    parameters:LineBufferParameters
):
    """Given 2D line buffer parameters and lists of input pixel values (in
3D format, see header comment), return the expected output as 4D list
(None entry for garbage). However, we only include entries on cycles
where the line buffer asserts valid. (e.g., if valid was high only on
cycles 0 and 2, then output[1] would be the list of windows emitted on
cycle 2).
    """
    p = parameters
    p.validate()

    # (row, col) -> pixel
    pixel_dict = {}
    y, x = 0, 0

    for cycle_i, cycle_input in enumerate(in_arrays):
        if len(cycle_input) != p.y_per_clk:
            raise IndexError("Expected {} rows, got {} on cycle {}".format(
                p.y_per_clk, len(cycle_input), cycle_i)
            )
        for row in cycle_input:
            if len(row) != p.x_per_clk:
                raise IndexError("Expected row length {}, not {}, on cycle {}"
                    .format(p.x_per_clk, len(row), cycle_i)
                )
            for pixel in row:
                pixel_dict[y, x] = pixel
                x += 1
                if x == p.image_x:
                    y += 1
                    x = 0

    window_count, parallelism, valid_count = p.internal_parameters()

    window_origins = [(yo, xo)
        for yo in range(p.origin_y, p.origin_y + p.image_y, p.stride_y)
        for xo in range(p.origin_x, p.origin_x + p.image_x, p.stride_x)
    ]

    window_y = p.window_y
    window_x = p.window_x

    expected = \
    [ # valid cycles
        [ # parallelism
            [ # rows
                [
                    pixel_dict.get((yo+yi, xo+xi))
                    for xi in range(0, window_x)
                ]
                for yi in range(0, window_y)
            ]
            for (yo, xo) in window_origins[parallelism*i:parallelism*(i+1)]
        ]
        for i in range(valid_count)
    ]
    return expected

def generate_test_sets_2D(
    random_generators,
    parameters: LineBufferParameters
):
    """Generate a list of test case data, one for each random generator
from the random_generators iterable passed.

Each entry is a 3D list in the input formatted specified in the header
comment, with values generated by one of the rng's passed in
random_generators (rng should be callable with no args).
    """

    image_area = parameters.image_y * parameters.image_x
    px_per_clk = parameters.y_per_clk * parameters.x_per_clk
    cycle_count = image_area // px_per_clk

    result = \
    [
        [
            [
                [
                    rng() for x in range(parameters.image_x)
                ]
                for y in range(parameters.image_y)
            ]
            for cycle in range(cycle_count)
        ]
        for rng in random_generators
    ]
    return result

def make_bit_generators():
    """Create a list of bit generators."""

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

    stdlib_rng = random.Random(20010106)
    def rng():
        return stdlib_rng.random() >= 0.5

    return [third_true, every_fifth_false, rng]

def make_int_generators(bit_width=8):
    """Create list of bit_width-int generators. The ints are actually
lists of bits."""

    mask = (1 << bit_width) - 1

    ascending_value = -1
    def ascending():
        nonlocal ascending_value
        ascending_value += 1
        return ascending_value & mask

    # Note: I don't think randrange is guaranteed to give the same
    # result across versions so I'm manually mapping float->int.
    stdlib_rng = random.Random(20150821)
    def rng():
        return int(stdlib_rng.random() * 0x80000000) & mask

    return [ascending, rng]

def fill_garbage_with_none(actual, expected):
    """Inspect the actual and expected args (in 4D output format as in
header comment) and wherever expected has a None entry (representing
garbage output expected), fill in the same entry of actual with
None. This way we can use == to compare actual and expected without
having a failure due to mismatched garbage outputs.
    """
    try:
        for i0, a0 in enumerate(expected):
            for i1, a1 in enumerate(a0):
                for i2, a2 in enumerate(a1):
                    for i3, a3 in enumerate(a2):
                        if a3 == None:
                            actual[i0][i1][i2][i3] = None
    except IndexError as e:
        raise AssertionError(f"Actual is missing output cycle {i0}, "
            f"window {i1}, pixel ({i2},{i3})") from e

def impl_test_2D_line_buffer(
    parameters: LineBufferParameters
):
    """Instantiate a simulated line buffer with the specified parameters
and run it on test data sets."""

    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    LineBufferDef = DefineTwoDimensionalLineBuffer(cirb, **parameters.as_kwargs())
    window_count, parallelism, valid_count = parameters.internal_parameters()
    window_x = parameters.window_x
    window_y = parameters.window_y

    sim = None

    # We have to treat int (as bit array) and bit line buffers
    # differently. Deal with these differences here (different random
    # data generator, need int2seq/seq2int for ints). Ugly but better
    # than 2 copies of this function.
    token = parameters.magma_type
    if token == Bit:
        generators = make_bit_generators()

        def set_value(bits):
            sim.set_value(LineBufferDef.I, bits, scope)

        def get_pixel(window_index, y, x):
            return sim.get_value(LineBufferDef.O[window_index][y][x], scope)
    else:
        try:
            bit_width = token.N
            if token.T != Bit:
                raise TypeError("Not a bit.")
        except Exception as e:
            raise TypeError("Type must be Bit or Array(n, Bit)") from e

        generators = make_int_generators(bit_width)

        def set_value(ints):
            f = lambda px: int2seq(px, bit_width)
            bit_arrays = [[f(px) for px in row] for row in ints]
            sim.set_value(LineBufferDef.I, bit_arrays, scope)

        def get_pixel(window_index, y, x):
            return seq2int(
                sim.get_value(LineBufferDef.O[window_index][y][x], scope)
            )

    for test_set in generate_test_sets_2D(generators, parameters):
        sim = CoreIRSimulator(LineBufferDef, LineBufferDef.CLK,
            context=cirb.context,
            namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"]
        )

        # List for recording sequence of valid outputs (in 4D format
        # in header, skipping entries on cycles where valid is not
        # asserted), and a helper function:
        # tick_sim_collect_outputs ticks the simulation, appending any
        # output received when the simulated module asserts valid.
        actual = []
        expected = expected_valid_outputs_2D(test_set, parameters)
        def tick_sim_collect_outputs():
            sim.evaluate()
            sim.advance_cycle()
            if sim.get_value(LineBufferDef.valid, scope):
                actual.append(
                    [ # Parallel output windows
                        [ # Rows
                            [ # Columns
                                get_pixel(window_index, y, x)
                                for x in range(window_x)
                            ]
                            for y in range(window_y)
                        ] for window_index in range(parallelism)
                    ]
                )

        for input_window in test_set:
            set_value(input_window)
            tick_sim_collect_outputs()

        # Wait for a little extra after the last input because the
        # line buffer may need a bit of extra time to emit the last
        # windows.
        extra_cycles = 16 + parameters.image_x * window_y + window_x
        for i in range(extra_cycles):
            if len(actual) >= len(expected): break
            tick_sim_collect_outputs()

        len_actual, len_expected = len(actual), len(expected)

        assert len_actual >= len_expected, \
            f"Got {len_actual} outputs (counts of valid asserts); expected " \
            f"{len_expected} outputs.\n" \
            f"Waited {extra_cycles} cycles after last input."

        fill_garbage_with_none(actual, expected)
        assert actual == expected, "Outputs don't match expected values."

# Test stub functions that call the actual implementation function
# with specified parameters.
def test_2D_bit_line_buffer_1_1_3_3_5_5_1_1_0_0():
    impl_test_2D_line_buffer(LineBufferParameters(
        magma_type=Bit,
        y_per_clk=1, x_per_clk=1,
        window_y=3, window_x=3,
        image_y=5, image_x=5,
        stride_y=1, stride_x=1,
        origin_y=0, origin_x=0
    ))

def test_2D_int4_line_buffer_1_1_3_3_5_5_1_1_0_0():
    impl_test_2D_line_buffer(LineBufferParameters(
        magma_type=Array(4, Bit),
        y_per_clk=1, x_per_clk=1,
        window_y=3, window_x=3,
        image_y=5, image_x=5,
        stride_y=1, stride_x=1,
        origin_y=0, origin_x=0
    ))
