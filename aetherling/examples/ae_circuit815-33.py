import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_TSeq(3, 0, ST_Int()).magma_repr()), 'O', Out(ST_TSeq(1, 2, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 0, ST_Int())]
        st_out_t = ST_TSeq(1, 2, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n30 = DefineConst(ST_TSeq(3, 0, ST_Int()), (1,1,1,), has_valid=True, delay=2)()
            wire(cls.valid_up, n30.valid_up)
            n33 = DefineMap2_T(3, 0, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True))()
            wire(n30.O, n33.I0)
            wire(cls.I, n33.I1)
            wire(n30.valid_down & cls.valid_up, n33.valid_up)
            n39 = DefineMap_T(3, 0, DefineMul_Atom(True))()
            wire(n33.O, n39.I)
            wire(n33.valid_down, n39.valid_up)
            n42 = DefineReduce_T(3, 0, DefineAdd_Atom(False))()
            wire(n39.O, n42.I)
            wire(n39.valid_down, n42.valid_up)
            n43 = DefineConst(ST_TSeq(1, 2, ST_Int()), (3,0,0,), has_valid=True, delay=7)()
            wire(cls.valid_up, n43.valid_up)
            n45 = DefineMap2_T(1, 2, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True))()
            wire(n42.O, n45.I0)
            wire(n43.O, n45.I1)
            wire(n42.valid_down & n43.valid_down, n45.valid_up)
            n51 = DefineMap_T(1, 2, DefineDiv_Atom(True))()
            wire(n45.O, n51.I)
            wire(n45.valid_down, n51.valid_up)
            wire(n51.O, cls.O)
            wire(n51.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['I', In(ST_TSeq(3, 0, ST_Int()).magma_repr()), 'O', Out(ST_TSeq(1, 2, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 0, ST_Int())]
        st_out_t = ST_TSeq(1, 2, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n85 = DefineConst(ST_TSeq(3, 0, ST_Int()), (1,1,1,), has_valid=True, delay=15)()
            wire(cls.valid_up, n85.valid_up)
            n88 = DefineMap2_T(3, 0, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True))()
            wire(n85.O, n88.I0)
            wire(cls.I, n88.I1)
            wire(n85.valid_down & cls.valid_up, n88.valid_up)
            n94 = DefineMap_T(3, 0, DefineMul_Atom(True))()
            wire(n88.O, n94.I)
            wire(n88.valid_down, n94.valid_up)
            n97 = DefineReduce_T(3, 0, DefineAdd_Atom(False))()
            wire(n94.O, n97.I)
            wire(n94.valid_down, n97.valid_up)
            n98 = DefineConst(ST_TSeq(1, 2, ST_Int()), (3,0,0,), has_valid=True, delay=20)()
            wire(cls.valid_up, n98.valid_up)
            n100 = DefineMap2_T(1, 2, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True))()
            wire(n97.O, n100.I0)
            wire(n98.O, n100.I1)
            wire(n97.valid_down & n98.valid_down, n100.valid_up)
            n106 = DefineMap_T(1, 2, DefineDiv_Atom(True))()
            wire(n100.O, n106.I)
            wire(n100.valid_down, n106.valid_up)
            wire(n106.O, cls.O)
            wire(n106.valid_down, cls.valid_down)
    return _Module_1

@cache_definition 
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "top"
        IO = ['hi', In(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_TSeq(1, 2, ST_Int()))).magma_repr()), 'O', Out(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 0, ST_TSeq(3, 0, ST_TSeq(1, 2, ST_Int())))]
        st_out_t = ST_TSeq(1, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_TSeq(1, 2, ST_Int()))), 1, has_valid=True)()
            wire(cls.hi, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_TT(3, 3, 0, 0, 1, ST_TSeq(1, 2, ST_Int()), has_valid=True)()
            wire(n1.O, n2.I)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_TT(3, 3, 0, 0, 1, ST_TSeq(1, 2, ST_Int()), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineMap2_T(3, 0, DefineMap2_T(3, 0, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(), has_valid=True))))()
            wire(n3.O, n4.I0)
            wire(n2.O, n4.I1)
            wire(n3.valid_down & n2.valid_down, n4.valid_up)
            n14 = DefineMap2_T(3, 0, DefineMap2_T(3, 0, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True))))()
            wire(n4.O, n14.I0)
            wire(n1.O, n14.I1)
            wire(n4.valid_down & n1.valid_down, n14.valid_up)
            n28 = DefineMap_T(3, 0, DefineMap_T(3, 0, DefineSerialize(3, 0, ST_Int())))()
            wire(n14.O, n28.I)
            wire(n14.valid_down, n28.valid_up)
            n53 = DefineMap_T(3, 0, DefineMap_T(3, 0, Module_0()))()
            wire(n28.O, n53.I)
            wire(n28.valid_down, n53.valid_up)
            n56 = DefineMap_T(3, 0, DefineDown_T(3, 0, 2, ST_TSeq(1, 2, ST_Int()), has_valid=True))()
            wire(n53.O, n56.I)
            wire(n53.valid_down, n56.valid_up)
            n57 = DefineShift_TT(3, 1, 0, 2, 1, ST_TSeq(1, 2, ST_Int()), has_valid=True)()
            wire(n56.O, n57.I)
            wire(n56.valid_down, n57.valid_up)
            n58 = DefineShift_TT(3, 1, 0, 2, 1, ST_TSeq(1, 2, ST_Int()), has_valid=True)()
            wire(n57.O, n58.I)
            wire(n57.valid_down, n58.valid_up)
            n59 = DefineMap2_T(3, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(), has_valid=True))))()
            wire(n58.O, n59.I0)
            wire(n57.O, n59.I1)
            wire(n58.valid_down & n57.valid_down, n59.valid_up)
            n69 = DefineMap2_T(3, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True))))()
            wire(n59.O, n69.I0)
            wire(n56.O, n69.I1)
            wire(n59.valid_down & n56.valid_down, n69.valid_up)
            n83 = DefineMap_T(3, 0, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int())))()
            wire(n69.O, n83.I)
            wire(n69.valid_down, n83.valid_up)
            n108 = DefineMap_T(3, 0, DefineMap_T(1, 2, Module_1()))()
            wire(n83.O, n108.I)
            wire(n83.valid_down, n108.valid_up)
            n109 = DefineDown_T(3, 0, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())), has_valid=True)()
            wire(n108.O, n109.I)
            wire(n108.valid_down, n109.valid_up)
            n112 = DefineMap_T(1, 2, DefineDown_T(1, 2, 0, ST_TSeq(1, 2, ST_Int()), has_valid=True))()
            wire(n109.O, n112.I)
            wire(n109.valid_down, n112.valid_up)
            n113 = DefineFIFO(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))), 1, has_valid=True)()
            wire(n112.O, n113.I)
            wire(n112.valid_down, n113.valid_up)
            n114 = DefineFIFO(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int()))), 1, has_valid=True)()
            wire(n113.O, n114.I)
            wire(n113.valid_down, n114.valid_up)
            wire(n114.O, cls.O)
            wire(n114.valid_down, cls.valid_down)
    return _Module_2

Main = Module_2
fault_inputs0 = [1,2,3,2,3,4,3,4,5,4,5,6,5,6,7,6,7,8,7,8,9,8,9,0,9,0,0]
fault_inputs0_valid = [True,False,False,True,False,False,True,False,False,True,False,False,True,False,False,True,False,False,True,False,False,True,False,False,True,False,False]
fault_output = [5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
fault_output_valid = [True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(27 + 41):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 27 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_TSeq(1, 2, ST_Int())))), 0)
            tester.print("hi: ")
            fault_helpers.print_nested_port(tester, tester.circuit.hi, num_nested_space_layers(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_TSeq(1, 2, ST_Int())))))
            tester.print("\n")
        tester.eval()
        if f_clk > 41:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))))
        tester.print("\n")
        if f_clk >= 41:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 41 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int())))), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
