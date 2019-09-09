from aetherling.space_time.space_time_types import *
from dataclasses import dataclass, replace
from typing import Any
import copy


@dataclass
class SharedDiffTypes:
    """
    The result of calling get_shared_and_diff_subtypes, which splits two types into their shard and different components
    """
    shared_inner: ST_Type
    shared_outer: ST_Type
    diff_input: ST_Type
    diff_output: ST_Type

@dataclass(frozen=True)
class ST_Tombstone:
    """
    A tombstone node. Present to end ST_Type trees that will be connected with other trees (like a diff with a shread_inner)
    """
    def length(self):
        return 1

    def time(self):
        return 1

    def port_width(self):
        return 1

    def magma_repr(self):
        raise Exception("shouldn't call magma_repr on tombstone.")


def get_shared_and_diff_subtypes(input_type: ST_Type, output_type: ST_Type) -> SharedDiffTypes:
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
        input_copy = replace(input_type, t=ST_Tombstone())
        output_copy = replace(output_type, t=ST_Tombstone())

        # this case is for shared outer
        if input_copy == output_copy:
            shared_outer = replace(input_copy, t=inner_shared_diff.shared_outer)
            return SharedDiffTypes(inner_shared_diff.shared_inner, shared_outer, inner_shared_diff.diff_input, inner_shared_diff.diff_output)
        # this case is for diff
        else:
            # if already did a shared_outer, then problem as this doesnt match pattern of shared_outer, diff, shared_inner
            # so return none
            if inner_shared_diff.shared_outer != ST_Tombstone():
                return None
            else:
                input_diff = replace(input_copy, inner_shared_diff.diff_input)
                output_diff = replace(output_copy, inner_shared_diff.diff_output)
                return SharedDiffTypes(inner_shared_diff.shared_inner, ST_Tombstone(), input_diff, output_diff)
    else:
        return None

