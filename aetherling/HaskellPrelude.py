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
            n4 = DefineAbs_Atom()()
            wire(cls.f_in, n4.I)
            wire(n4.O, cls.O)
    return _Module_0

@cache_definition
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['f_in1', In(ST_Int().magma_repr()), 'f_in2', In(ST_Int().magma_repr()), 'O', Out(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False)
        st_in_t = [ST_Int(), ST_Int()]
        st_out_t = ST_Int()
        binary_op = True
        @classmethod
        def definition(cls):
            n2 = DefineAtomTupleCreator(ST_Int(), ST_Int())()
            wire(cls.f_in1, n2.I0)
            wire(cls.f_in2, n2.I1)
            wire(n2.O, cls.O)
    return _Module_1

@cache_definition
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "Module_2"
        IO = ['hi', In(ST_SSeq(4, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(4, ST_Atom_Tuple(ST_Int(), ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False)
        st_in_t = ST_SSeq(4, ST_Int())
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n6 = DefineMap_S(4, Module_0())()
            wire(cls.hi, n6.f_in)
            # why did Module_1()() have extra ()?
            n7 = DefineMap2_S(4, Module_1())()
            wire(n6.O, n7.f_in1)
            wire(n6.O, n7.f_in2)
            wire(n7.O, cls.O)
    return _Module_2

Main = Module_2()
#fault_helpers.compile(Main)
fault_inputs0 = [[0,1,2,3]]
fault_output = [[(0,0),(1,1),(2,2),(3,3)]]
if __name__ == '__main__':
    mod = Main
    tester = fault.Tester(mod, mod.CLK)
    for f_clk in range(1):
        #fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk])
        tester.circuit.hi = fault_inputs0[f_clk]
        tester.eval()
        #fault_helpers.expect_nested_port(tester.circuit.O, fault_output[f_clk])
        tester.circuit.O.expect(fault_output[f_clk])
        tester.step(2)
    fault_helpers.compile_and_run(tester)

