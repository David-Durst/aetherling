from dataclasses import dataclass, replace
from typing import List

@dataclass(frozen=True)
class Resources_Consumed():
    num_pe_tiles: int

empty_resources = Resources_Consumed(0)

def constant_generator(constants: List[int], resources: Resources_Consumed) -> (List[int], Resources_Consumed):
    """
    A PE configured to generate constants.
    :param constants:
    :return:
    """
    return (constants, replace(resources, num_pe_tiles=resources.num_pe_tiles + 1))

def adder_tile(int_width: int, inputs0: List[int], inputs1: List[int], resources: Resources_Consumed) -> List[int]:
    """
    A PE configured in adder mode. It can add 32 bits every clock.
    It can add either 4 8-bit ints, 2 16-bit ints, or 1 32-bii int
    :param int_width:  The width of the inputs
    :param inputs0:  The inputs for the left half of add
    :param inputs1:  The inputs for the right half of add
    :return:
    """
    output = []
    if int_width == 8:
        assert len(inputs0) == 4
        assert len(inputs1) == 4
    elif int_width == 16:
        assert len(inputs0) == 2
        assert len(inputs1) == 2
    elif int_width == 32:
        assert len(inputs0) == 1
        assert len(inputs1) == 1
    for i in len(inputs0):
        output += inputs0[i] + inputs1[i]
    return (output, replace(resources, num_pe_tiles=resources.num_pe_tiles + 1))

(c0, c0_r) = constant_generator([1,2,3,4], empty_resources)
(c1, c1_r) = constant_generator([4,5,6,7], c0_r)
(x, x_r) = adder_tile(8, c0, c1, c1_r)
(y, y_r) = adder_tile(8, x, x, y_r)

