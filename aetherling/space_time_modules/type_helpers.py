from aetherling.space_time_modules.space_time_types import *
from dataclasses import dataclass
from typing import Any
import copy


@dataclass
class SharedDiffTypes:
    """
    The result of calling get_shared_and_diff_subtypes, which splits two types into their shard and different components
    """
    shared_inner: Any
    shared_outer: Any
    diff_input: Any
    diff_output: Any

@dataclass
class ST_Tombstone:
    """
    A tombstone node. Present to end ST_Type trees that will be connected with other trees (like a diff with a shread_inner)
    """
    def length(self):
        return 1

    def time(self):
        return 1


def get_shared_and_diff_subtypes(input_type, output_type):
    """
    Given two types that take equal amount of time, have the same length, but have different
    sseq/tseq outer components, split the types into a shared core and different outer components

    :param input_type:
    :param output_type:
    :return: None if no shared base, otherwise SharedDiffTypes
    """
    # this case is for shared inner
    if input_type == output_type:
        return SharedDiffTypes(input_type, ST_Tombstone(), ST_Tombstone(), ST_Tombstone())
    elif is_nested(input_type) and is_nested(output_type):
        inner_shared_diff = get_shared_and_diff_subtypes(input_type.t, output_type.t)

        # replace all inner parts of input and output for comparing this layer
        input_copy = copy.copy(input_type)
        output_copy = copy.copy(output_type)
        input_copy.t = ST_Tombstone()
        output_copy.t = ST_Tombstone()

        # this case is for shared outer
        if input_copy == output_copy:
            shared_outer = copy.copy(input_copy)
            shared_outer.t = inner_shared_diff.shared_outer
            return SharedDiffTypes(inner_shared_diff.shared_inner, shared_outer, inner_shared_diff.diff_input, inner_shared_diff.diff_output)
        # this case is for diff
        else:
            # if already did a shared_outer, then problem as this doesnt match pattern of shared_outer, diff, shared_inner
            # so return none
            if inner_shared_diff.shared_outer != ST_Tombstone():
                return None
            else:
                input_diff = copy.copy(input_copy)
                input_diff.t = inner_shared_diff.diff_input
                output_diff = copy.copy(output_copy)
                output_diff.t = inner_shared_diff.diff_output
                return SharedDiffTypes(inner_shared_diff.shared_inner, ST_Tombstone(), input_diff, output_diff)
    else:
        return None

