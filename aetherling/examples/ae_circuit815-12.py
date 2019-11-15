import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_SSeq(3, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(1, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(3, ST_Int())]
        st_out_t = ST_SSeq(1, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n32 = DefineConst(ST_SSeq(3, ST_Int()), ((1,1,1,),), has_valid=True, delay=1)()
            wire(cls.valid_up, n32.valid_up)
            n35 = DefineMap2_S(3, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True)()
            wire(n32.O, n35.I0)
            wire(cls.I, n35.I1)
            wire(n32.valid_down & cls.valid_up, n35.valid_up)
            n41 = DefineMap_S(3, DefineMul_Atom(True),True)()
            wire(n35.O, n41.I)
            wire(n35.valid_down, n41.valid_up)
            n44 = DefineReduce_S(3, DefineAdd_Atom(False), has_valid=True)()
            wire(n41.O, n44.I)
            wire(n41.valid_down, n44.valid_up)
            n45 = DefineConst(ST_SSeq(1, ST_Int()), (3,), has_valid=True, delay=4)()
            wire(cls.valid_up, n45.valid_up)
            n47 = DefineMap2_S(1, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True)()
            wire(n44.O, n47.I0)
            wire(n45.O, n47.I1)
            wire(n44.valid_down & n45.valid_down, n47.valid_up)
            n53 = DefineMap_S(1, DefineDiv_Atom(True),True)()
            wire(n47.O, n53.I)
            wire(n47.valid_down, n53.valid_up)
            wire(n53.O, cls.O)
            wire(n53.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['I', In(ST_SSeq(3, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(1, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(3, ST_Int())]
        st_out_t = ST_SSeq(1, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n89 = DefineConst(ST_SSeq(3, ST_Int()), ((1,1,1,),), has_valid=True, delay=7)()
            wire(cls.valid_up, n89.valid_up)
            n92 = DefineMap2_S(3, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True)()
            wire(n89.O, n92.I0)
            wire(cls.I, n92.I1)
            wire(n89.valid_down & cls.valid_up, n92.valid_up)
            n98 = DefineMap_S(3, DefineMul_Atom(True),True)()
            wire(n92.O, n98.I)
            wire(n92.valid_down, n98.valid_up)
            n101 = DefineReduce_S(3, DefineAdd_Atom(False), has_valid=True)()
            wire(n98.O, n101.I)
            wire(n98.valid_down, n101.valid_up)
            n102 = DefineConst(ST_SSeq(1, ST_Int()), (3,), has_valid=True, delay=10)()
            wire(cls.valid_up, n102.valid_up)
            n104 = DefineMap2_S(1, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True)()
            wire(n101.O, n104.I0)
            wire(n102.O, n104.I1)
            wire(n101.valid_down & n102.valid_down, n104.valid_up)
            n110 = DefineMap_S(1, DefineDiv_Atom(True),True)()
            wire(n104.O, n110.I)
            wire(n104.valid_down, n110.valid_up)
            wire(n110.O, cls.O)
            wire(n110.valid_down, cls.valid_down)
    return _Module_1

@cache_definition 
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "top"
        IO = ['hi', In(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_SSeq(1, ST_Int()))).magma_repr()), 'O', Out(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int()))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 0, ST_TSeq(3, 0, ST_SSeq(1, ST_Int())))]
        st_out_t = ST_TSeq(1, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int())))
        IO += ['fst_lb_out', Out(ST_SSeq(3, ST_Int()).magma_repr())]
        IO += ['fst_lb_valid_down', Out(Bit)]
        IO += ['fst_conv_out', Out(ST_SSeq(1, ST_Int()).magma_repr())]
        IO += ['fst_conv_valid_down', Out(Bit)]
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_SSeq(1, ST_Int()))), 1, has_valid=True)()
            wire(cls.hi, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_TT(3, 3, 0, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n1.O, n2.I)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_TT(3, 3, 0, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineMap2_T(3, 0, DefineMap2_T(3, 0, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True)))()
            wire(n3.O, n4.I0)
            wire(n2.O, n4.I1)
            wire(n3.valid_down & n2.valid_down, n4.valid_up)
            n14 = DefineMap2_T(3, 0, DefineMap2_T(3, 0, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True)))()
            wire(n4.O, n14.I0)
            wire(n1.O, n14.I1)
            wire(n4.valid_down & n1.valid_down, n14.valid_up)
            n30 = DefineMap_T(3, 0, DefineMap_T(3, 0, DefineRemove_1_S(DefineSTupleToSSeq(ST_Int(), 3, has_valid=True),True)))()
            wire(n14.O, n30.I)
            wire(n14.valid_down, n30.valid_up)
            wire(n30.valid_down, cls.fst_lb_valid_down)
            wire(n30.O, cls.fst_lb_out)
            n55 = DefineMap_T(3, 0, DefineMap_T(3, 0, Module_0()))()
            wire(n30.O, n55.I)
            wire(n30.valid_down, n55.valid_up)
            wire(n55.valid_down, cls.fst_conv_valid_down)
            wire(n55.O, cls.fst_conv_out)
            n58 = DefineMap_T(3, 0, DefineDown_T(3, 0, 2, ST_SSeq(1, ST_Int()), has_valid=True))()
            wire(n55.O, n58.I)
            wire(n55.valid_down, n58.valid_up)
            n59 = DefineShift_TT(3, 1, 0, 2, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n58.O, n59.I)
            wire(n58.valid_down, n59.valid_up)
            n60 = DefineShift_TT(3, 1, 0, 2, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n59.O, n60.I)
            wire(n59.valid_down, n60.valid_up)
            n61 = DefineMap2_T(3, 0, DefineMap2_T(1, 2, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True)))()
            wire(n60.O, n61.I0)
            wire(n59.O, n61.I1)
            wire(n60.valid_down & n59.valid_down, n61.valid_up)
            n71 = DefineMap2_T(3, 0, DefineMap2_T(1, 2, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True)))()
            wire(n61.O, n71.I0)
            wire(n58.O, n71.I1)
            wire(n61.valid_down & n58.valid_down, n71.valid_up)
            n87 = DefineMap_T(3, 0, DefineMap_T(1, 2, DefineRemove_1_S(DefineSTupleToSSeq(ST_Int(), 3, has_valid=True),True)))()
            wire(n71.O, n87.I)
            wire(n71.valid_down, n87.valid_up)
            n112 = DefineMap_T(3, 0, DefineMap_T(1, 2, Module_1()))()
            wire(n87.O, n112.I)
            wire(n87.valid_down, n112.valid_up)
            n113 = DefineDown_T(3, 0, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int())), has_valid=True)()
            wire(n112.O, n113.I)
            wire(n112.valid_down, n113.valid_up)
            n116 = DefineMap_T(1, 2, DefineDown_T(1, 2, 0, ST_SSeq(1, ST_Int()), has_valid=True))()
            wire(n113.O, n116.I)
            wire(n113.valid_down, n116.valid_up)
            n117 = DefineFIFO(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int()))), 1, has_valid=True)()
            wire(n116.O, n117.I)
            wire(n116.valid_down, n117.valid_up)
            n118 = DefineFIFO(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int()))), 1, has_valid=True)()
            wire(n117.O, n118.I)
            wire(n117.valid_down, n118.valid_up)
            wire(n118.O, cls.O)
            wire(n118.valid_down, cls.valid_down)
    return _Module_2

Main = Module_2
fault_inputs0 = [1,2,3,4,5,6,7,8,9]
fault_inputs0_valid = [True,True,True,True,True,True,True,True,True]
fault_output = [5,0,0,0,0,0,0,0,0]
fault_output_valid = [True,False,False,False,False,False,False,False,False]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(9 + 19):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 9 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_SSeq(1, ST_Int())))), 0)
            tester.print("hi: ")
            fault_helpers.print_nested_port(tester, tester.circuit.hi, num_nested_space_layers(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_SSeq(1, ST_Int())))))
            tester.print("\n")
        tester.eval()
        if f_clk > 19:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int())))))
        tester.print("\n")
        tester.print("fst_lb_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.fst_lb_out, num_nested_space_layers(ST_SSeq(3, ST_Int())))
        tester.print("\n")
        tester.print("fst_lb_valid_down: %d\n", tester.circuit.fst_lb_valid_down)
        tester.print("fst_conv_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.fst_conv_out, num_nested_space_layers(ST_SSeq(1, ST_Int())))
        tester.print("\n")
        tester.print("fst_conv_valid_down: %d\n", tester.circuit.fst_conv_valid_down)
        if f_clk >= 19:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 19 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_SSeq(1, ST_Int())))), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
