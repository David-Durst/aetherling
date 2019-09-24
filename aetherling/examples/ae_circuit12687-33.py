import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *


@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['f_in', In(ST_Int().magma_repr()), 'O', Out(ST_Int().magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_Int()
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineAbs_Atom(True)()
            wire(cls.f_in, n1.I)
            wire(cls.valid_up, n1.valid_up)
            wire(n1.O, cls.O)
            wire(n1.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['hi', In(ST_TSeq(1, 3, ST_Int()).magma_repr()), 'O', Out(ST_TSeq(4, 0, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_TSeq(1, 3, ST_Int())
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n3 = DefineMap_T(1, 3, Module_0())()
            wire(cls.hi, n3.f_in)
            wire(cls.valid_up, n3.valid_up)
            n4 = DefineUp_T(4, 0, ST_Int(), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            wire(n4.O, cls.O)
            wire(n4.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = [2,0,0,0]
fault_output = [2,2,2,2]
fault_output_valid = [True,True,True,True]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    tester.circuit.valid_in = 1
    for f_clk in range(4):
        tester.print('clk: {}\n'.format(f_clk))
        tester.circuit.hi = fault_inputs0[f_clk]
        tester.eval()
        if fault_output_valid[f_clk]:
            tester.circuit.O.expect(fault_output[f_clk])
        tester.step(2)
    fault_helpers.compile_and_run(tester)
