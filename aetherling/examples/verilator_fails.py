import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_TSeq(1, 1, ST_SSeq(3, ST_Int())).magma_repr()), 'O', Out(ST_TSeq(1, 1, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(1, 1, ST_SSeq(3, ST_Int()))]
        st_out_t = ST_TSeq(1, 1, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n181 = DefineMap_T(1, 1, DefineReduce_S(3, DefineAdd_Atom(False), has_valid=True))()
            wire(cls.I, n181.I)
            wire(cls.valid_up, n181.valid_up)
            n184 = DefineReduce_T(1, 1, DefineMap_S(1, DefineAdd_Atom(False),False))()
            wire(n181.O, n184.I)
            wire(n181.valid_down, n184.valid_up)
            n185 = DefineReshape_ST(ST_TSeq(1, 1, ST_SSeq(1, ST_Int())), ST_TSeq(1, 1, ST_Int()), has_valid=True)()
            wire(n184.O, n185.I)
            wire(n184.valid_down, n185.valid_up)
            wire(n185.O, cls.O)
            wire(n185.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['I', In(ST_TSeq(3, 1, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(3, ST_Int())))).magma_repr()), 'O', Out(ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int()))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 1, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(3, ST_Int()))))]
        st_out_t = ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int())))
        binary_op = False
        @classmethod
        def definition(cls):
            n151 = DefineConst(ST_TSeq(3, 1, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(3, ST_Int())))), ((0,1,0,),(1,2,1,),(1,2,1,),(0,1,0,),(0,1,0,),(0,0,0,),(0,0,0,),(0,0,0,),), has_valid=True, delay=3)()
            wire(cls.valid_up, n151.valid_up)
            n153 = DefineMap2_T(3, 1, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(3, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True)),True))()
            wire(cls.I, n153.I0)
            wire(n151.O, n153.I1)
            wire(cls.valid_up & n151.valid_down, n153.valid_up)
            n174 = DefineMap_T(3, 1, DefineMap_S(1, DefineMap_T(1, 1, DefineMap_S(3, DefineLShift_Atom(True),True)),True))()
            wire(n153.O, n174.I)
            wire(n153.valid_down, n174.valid_up)
            n187 = DefineMap_T(3, 1, DefineMap_S(1, Module_0(),True))()
            wire(n174.O, n187.I)
            wire(n174.valid_down, n187.valid_up)
            n194 = DefineMap_T(3, 1, DefineReduce_S(1, DefineMap_T(1, 1, DefineAdd_Atom(False)), has_valid=True))()
            wire(n187.O, n194.I)
            wire(n187.valid_down, n194.valid_up)
            n197 = DefineReduce_T(3, 1, DefineMap_S(1, DefineMap_T(1, 1, DefineAdd_Atom(False)),False))()
            wire(n194.O, n197.I)
            wire(n194.valid_down, n197.valid_up)
            n198 = DefineConst(ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int()))), (4,0,0,0,0,0,0,0,), has_valid=True, delay=11)()
            wire(cls.valid_up, n198.valid_up)
            n200 = DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True)),True))()
            wire(n197.O, n200.I0)
            wire(n198.O, n200.I1)
            wire(n197.valid_down & n198.valid_down, n200.valid_up)
            n216 = DefineMap_T(1, 3, DefineMap_S(1, DefineMap_T(1, 1, DefineRShift_Atom(True)),True))()
            wire(n200.O, n216.I)
            wire(n200.valid_down, n216.valid_up)
            wire(n216.O, cls.O)
            wire(n216.valid_down, cls.valid_down)
    return _Module_1

@cache_definition 
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "top"
        IO = ['I', In(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int()))))).magma_repr()), 'O', Out(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int())))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))))]
        st_out_t = ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int()))))
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int()))))), 1, has_valid=True)()
            wire(cls.I, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_S(16, 4, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n1.O, n2.I)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_S(16, 4, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineShift_S(16, 1, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            n5 = DefineShift_S(16, 1, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n4.O, n5.I)
            wire(n4.valid_down, n5.valid_up)
            n6 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True)),True)),True)()
            wire(n5.O, n6.I0)
            wire(n4.O, n6.I1)
            wire(n5.valid_down & n4.valid_down, n6.valid_up)
            n22 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True)),True)),True)()
            wire(n6.O, n22.I0)
            wire(n3.O, n22.I1)
            wire(n6.valid_down & n3.valid_down, n22.valid_up)
            n44 = DefineMap_S(16, DefineMap_T(1, 3, DefineMap_S(1, DefineReshape_ST(ST_TSeq(1, 1, ST_SSeq(1, ST_SSeq_Tuple(3, ST_Int()))), ST_TSeq(1, 1, ST_SSeq(3, ST_Int())), has_valid=True),True)),True)()
            wire(n22.O, n44.I)
            wire(n22.valid_down, n44.valid_up)
            n45 = DefineShift_S(16, 1, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n2.O, n45.I)
            wire(n2.valid_down, n45.valid_up)
            n46 = DefineShift_S(16, 1, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n45.O, n46.I)
            wire(n45.valid_down, n46.valid_up)
            n47 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True)),True)),True)()
            wire(n46.O, n47.I0)
            wire(n45.O, n47.I1)
            wire(n46.valid_down & n45.valid_down, n47.valid_up)
            n63 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True)),True)),True)()
            wire(n47.O, n63.I0)
            wire(n2.O, n63.I1)
            wire(n47.valid_down & n2.valid_down, n63.valid_up)
            n85 = DefineMap_S(16, DefineMap_T(1, 3, DefineMap_S(1, DefineReshape_ST(ST_TSeq(1, 1, ST_SSeq(1, ST_SSeq_Tuple(3, ST_Int()))), ST_TSeq(1, 1, ST_SSeq(3, ST_Int())), has_valid=True),True)),True)()
            wire(n63.O, n85.I)
            wire(n63.valid_down, n85.valid_up)
            n86 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineSSeqTupleCreator(ST_TSeq(1, 1, ST_SSeq(3, ST_Int())), has_valid=True),True)),True)()
            wire(n44.O, n86.I0)
            wire(n85.O, n86.I1)
            wire(n44.valid_down & n85.valid_down, n86.valid_up)
            n96 = DefineShift_S(16, 1, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n1.O, n96.I)
            wire(n1.valid_down, n96.valid_up)
            n97 = DefineShift_S(16, 1, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))), has_valid=True)()
            wire(n96.O, n97.I)
            wire(n96.valid_down, n97.valid_up)
            n98 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True)),True)),True)()
            wire(n97.O, n98.I0)
            wire(n96.O, n98.I1)
            wire(n97.valid_down & n96.valid_down, n98.valid_up)
            n114 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineMap2_T(1, 1, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True)),True)),True)()
            wire(n98.O, n114.I0)
            wire(n1.O, n114.I1)
            wire(n98.valid_down & n1.valid_down, n114.valid_up)
            n136 = DefineMap_S(16, DefineMap_T(1, 3, DefineMap_S(1, DefineReshape_ST(ST_TSeq(1, 1, ST_SSeq(1, ST_SSeq_Tuple(3, ST_Int()))), ST_TSeq(1, 1, ST_SSeq(3, ST_Int())), has_valid=True),True)),True)()
            wire(n114.O, n136.I)
            wire(n114.valid_down, n136.valid_up)
            n137 = DefineMap2_S(16, DefineMap2_T(1, 3, DefineMap2_S(1, DefineSSeqTupleAppender(ST_TSeq(1, 1, ST_SSeq(3, ST_Int())), 2, has_valid=True),True)),True)()
            wire(n86.O, n137.I0)
            wire(n136.O, n137.I1)
            wire(n86.valid_down & n136.valid_down, n137.valid_up)
            n149 = DefineMap_S(16, DefineReshape_ST(ST_TSeq(1, 3, ST_SSeq(1, ST_SSeq_Tuple(3, ST_TSeq(1, 1, ST_SSeq(3, ST_Int()))))), ST_TSeq(3, 1, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(3, ST_Int())))), has_valid=True),True)()
            wire(n137.O, n149.I)
            wire(n137.valid_down, n149.valid_up)
            n217 = DefineMap_S(16, Module_1(),True)()
            wire(n149.O, n217.I)
            wire(n149.valid_down, n217.valid_up)
            n218 = DefineFIFO(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int())))), 1, has_valid=True)()
            wire(n217.O, n218.I)
            wire(n217.valid_down, n218.valid_up)
            n219 = DefineFIFO(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int())))), 1, has_valid=True)()
            wire(n218.O, n219.I)
            wire(n218.valid_down, n219.valid_up)
            wire(n219.O, cls.O)
            wire(n219.valid_down, cls.valid_down)
    return _Module_2

Main = Module_2
fault_inputs0 = [[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,0],[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,0],[3,4,5,6,7,8,9,10,11,12,13,14,15,16,0,0],[3,4,5,6,7,8,9,10,11,12,13,14,15,16,0,0],[4,5,6,7,8,9,10,11,12,13,14,15,16,0,0,0],[4,5,6,7,8,9,10,11,12,13,14,15,16,0,0,0],[5,6,7,8,9,10,11,12,13,14,15,16,0,0,0,0]]
fault_inputs0_valid = [True,False,False,False,False,False,False,False]
fault_output = [[253,253,253,253,253,253,253,253,253,253,6,7,253,253,10,11],[253,253,253,253,253,253,253,253,253,6,7,253,253,10,11,0],[253,253,253,253,253,253,253,253,253,6,7,253,253,10,11,0],[253,253,253,253,253,253,253,253,6,7,253,253,10,11,0,0],[253,253,253,253,253,253,253,253,6,7,253,253,10,11,0,0],[253,253,253,253,253,253,253,6,7,253,253,10,11,0,0,0],[253,253,253,253,253,253,253,6,7,253,253,10,11,0,0,0],[253,253,253,253,253,253,6,7,253,253,10,11,0,0,0,0]]
fault_output_valid = [True,False,False,False,False,False,False,False]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(8 + 13):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 8 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.I, fault_inputs0[f_clk], num_nested_space_layers(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))))), 0)
            tester.print("I: ")
            fault_helpers.print_nested_port(tester, tester.circuit.I, num_nested_space_layers(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_SSeq(1, ST_Int())))))))
            tester.print("\n")
        tester.eval()
        if f_clk > 13:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int()))))))
        tester.print("\n")
        if f_clk >= 13:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 13 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_SSeq(16, ST_TSeq(1, 3, ST_SSeq(1, ST_TSeq(1, 1, ST_Int()))))), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
