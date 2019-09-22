import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *


@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['f_in', In(ST_Int().magma_repr()), 'O', Out(ST_Int().magma_repr())] + ClockInterface(has_ce=False,has_reset=False)
        st_in_t = ST_Int()
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineAbs_Atom()()
            wire(cls.f_in, n1.I)
            wire(n1.O, cls.O)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['hi', In(ST_TSeq(4, 0, ST_Int()).magma_repr()), 'O', Out(ST_TSeq(4, 0, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False)
        st_in_t = ST_TSeq(4, 0, ST_Int())
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n3 = DefineMap_T(4, 0, Module_0())()
            wire(cls.hi, n3.f_in)
            wire(n3.O, cls.O)
    return _Module_1

Main = Module_1
fault_inputs0 = [[0],[-1],[2],[3]]
fault_output = [[0],[1],[2],[3]]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    for f_clk in range(4):
        tester.circuit.hi = fault_inputs0[f_clk]
        tester.eval()
        tester.circuit.O.expect(fault_output[f_clk])
        tester.step(2)
    fault_helpers.compile_and_run(tester)
