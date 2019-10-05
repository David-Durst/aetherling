from aetherling.modules.term_any_type import DefineTermAnyType
from magma import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_term_empty():
    term = DefineTermAnyType(Array[0, Array[8, Bit]])

    tester = fault.Tester(term)

    tester.circuit.valid_up = 1
    if False:
        compile_and_run(tester)
