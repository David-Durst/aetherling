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
from aetherling.modules.native_linebuffer import OneDimensionalLineBuffer, \
    DefineOneDimensionalLineBuffer
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
                + str(self.as_kwarg()))

        if self.y_per_clk != 1:
            raise ValueError("Need y_per_clk (px/clk height) to be 1.")

        a_divides_b = lambda a, b: b % a == 0

        if not all(a_divides_b for (a, b) in [(self.y_per_clk, self.image_y),
                                              (self.x_per_clk, self.image_x),
                                              (self.stride_y, self.image_y),
                                              (self.stride_x, self.image_x)]):
            raise ValueError("Need input px/clk width (height) and\n"
                "stride width (height) to divide image width (height) for\n"
                + str(self.as_kwarg()))

        window_throughput = (self.x_per_clk * self.y_per_clk /
                             (self.stride_x * self.stride_y))

        stride_area = self.stride_y * self.stride_x
        px_per_clk_area = self.y_per_clk * self.x_per_clk

        if (not a_divides_b(stride_area, px_per_clk_area) and
            not a_divides_b(px_per_clk_area, stride_area)):
            # i.e. window throughput needs to be int or reciprocal of int.
            raise ValueError("Need integer or integer-reciprocal window "
                f"throughput.\nWindow throughput={window_throughput} in\n"+
                str(self.as_kwarg()))

        if (self.origin_y > 0 or self.origin_y <= -self.window_y or
            self.origin_x > 0 or self.origin_x <= -self.window_x):

            raise ValueError("Origin must be between (0, 0) and "
                "(-window_y+1, -window_x+1)\n" + str(self.as_kwarg()))

    def as_kwarg(self):
        """Return parameters held within as a dict suitable for passing as
kwargs (using **) to TwoDimensionalLineBuffer.
        """
        return {
            "pixel_type":self.magma_type,
            "pixel_per_clock":(self.y_per_clk, self.x_per_clk),
            "window_width":(self.window_y, self.window_x),
            "image_size":(self.image_y, self.image_x),
            "output_stride":(self.stride_y, self.stride_x),
            "origin":(self.origin_y, self.origin_x),
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
