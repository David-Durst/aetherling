import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m
import json


@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int()))).magma_repr()),'O', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        IO += ['mod0reshape', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_Int())).magma_repr())]
        IO += ['mod0mul', Out(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int()))).magma_repr())]
        IO += ['mod0reduce0out', Out(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(1, ST_Int()))).magma_repr())]
        IO += ['mod0reduce1out', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int()))).magma_repr())]
        IO += ['mod0reduce2out', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int()))).magma_repr())]
        IO += ['mod0reduce2valid_up', Out(Bit)]
        st_in_t = [ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int())))]
        st_out_t = ST_TSeq(1, 0, ST_SSeq(1, ST_Int()))
        binary_op = False
        @classmethod
        def definition(cls):
            n157 = DefineConst(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int()))), ((0,1,0,1,2,1,0,1,0,),), has_valid=True, delay=1)()
            wire(cls.valid_up, n157.valid_up)
            n103 = DefineMap2_T(1, 0, DefineMap2_S(3, DefineMap2_S(3, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True),True))()
            wire(cls.I, n103.I0)
            wire(n157.O, n103.I1)
            wire(cls.valid_up & n157.valid_down, n103.valid_up)
            n119 = DefineMap_T(1, 0, DefineMap_S(3, DefineMap_S(3, DefineLShift_Atom(True),True),True))()
            wire(n119.O, cls.mod0mul)
            wire(n103.O, n119.I)
            wire(n103.valid_down, n119.valid_up)
            n126 = DefineMap_T(1, 0, DefineMap_S(3, DefineReduce_S(3, DefineAdd_Atom(False), has_valid=True),True))()
            wire(n119.O, n126.I)
            wire(n126.O, cls.mod0reduce0out)
            wire(n119.valid_down, n126.valid_up)
            n133 = DefineMap_T(1, 0, DefineReduce_S(3, DefineMap_S(1, DefineAdd_Atom(False),False), has_valid=True))()
            wire(n126.O, n133.I)
            wire(n133.O, cls.mod0reduce1out)
            wire(n126.valid_down, n133.valid_up)
            n136 = DefineReduce_T(1, 0, DefineMap_S(1, DefineMap_S(1, DefineAdd_Atom(False),False),False))()
            wire(n133.O, n136.I)
            wire(n136.O, cls.mod0reduce2out)
            wire(n133.valid_down, cls.mod0reduce2valid_up)
            wire(n133.valid_down, n136.valid_up)
            n137 = DefineReshape_ST(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int()))), ST_TSeq(1, 0, ST_SSeq(1, ST_Int())), has_valid=True)()
            wire(cls.mod0reshape, n137.O)
            wire(n136.O, n137.I)
            wire(n136.valid_down, n137.valid_up)
            n158 = DefineConst(ST_TSeq(1, 0, ST_SSeq(1, ST_Int())), (4,), has_valid=True, delay=4)()
            wire(cls.valid_up, n158.valid_up)
            n140 = DefineMap2_T(1, 0, DefineMap2_S(1, DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True),True))()
            wire(n137.O, n140.I0)
            wire(n158.O, n140.I1)
            wire(n137.valid_down & n158.valid_down, n140.valid_up)
            n151 = DefineMap_T(1, 0, DefineMap_S(1, DefineRShift_Atom(True),True))()
            wire(n140.O, n151.I)
            wire(n140.valid_down, n151.valid_up)
            wire(n151.O, cls.O)
            wire(n151.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "top"
        IO = ['I', In(ST_TSeq(16, 0, ST_SSeq(1, ST_Int())).magma_repr()),'O', Out(ST_TSeq(16, 0, ST_SSeq(1, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(16, 0, ST_SSeq(1, ST_Int()))]
        st_out_t = ST_TSeq(16, 0, ST_SSeq(1, ST_Int()))
        IO += ['mod0in', Out(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int()))).magma_repr())]
        IO += ['mod0reshape', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_Int())).magma_repr())]
        IO += ['mod0mul', Out(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int()))).magma_repr())]
        IO += ['mod0reduce0out', Out(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(1, ST_Int()))).magma_repr())]
        IO += ['mod0reduce1out', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int()))).magma_repr())]
        IO += ['mod0reduce2out', Out(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int()))).magma_repr())]
        IO += ['mod0reduce2valid_up', Out(Bit)]
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_TSeq(16, 0, ST_SSeq(1, ST_Int())), 1, has_valid=True)()
            wire(cls.I, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_T(16, 0, 4, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n1.O, n2.I)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_T(16, 0, 4, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineShift_T(16, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            n5 = DefineShift_T(16, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n4.O, n5.I)
            wire(n4.valid_down, n5.valid_up)
            n6 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True))()
            wire(n5.O, n6.I0)
            wire(n4.O, n6.I1)
            wire(n5.valid_down & n4.valid_down, n6.valid_up)
            n13 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True))()
            wire(n6.O, n13.I0)
            wire(n3.O, n13.I1)
            wire(n6.valid_down & n3.valid_down, n13.valid_up)
            n22 = DefineMap_T(16, 0, DefinePartition_S(1, 1, ST_SSeq_Tuple(3, ST_Int()), has_valid=True))()
            wire(n13.O, n22.I)
            wire(n13.valid_down, n22.valid_up)
            n29 = DefineMap_T(16, 0, DefineMap_S(1, DefineRemove_1_S(DefineSTupleToSSeq(ST_Int(), 3, has_valid=True),True),True))()
            wire(n22.O, n29.I)
            wire(n22.valid_down, n29.valid_up)
            n30 = DefineShift_T(16, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n2.O, n30.I)
            wire(n2.valid_down, n30.valid_up)
            n31 = DefineShift_T(16, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n30.O, n31.I)
            wire(n30.valid_down, n31.valid_up)
            n32 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True))()
            wire(n31.O, n32.I0)
            wire(n30.O, n32.I1)
            wire(n31.valid_down & n30.valid_down, n32.valid_up)
            n39 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True))()
            wire(n32.O, n39.I0)
            wire(n2.O, n39.I1)
            wire(n32.valid_down & n2.valid_down, n39.valid_up)
            n48 = DefineMap_T(16, 0, DefinePartition_S(1, 1, ST_SSeq_Tuple(3, ST_Int()), has_valid=True))()
            wire(n39.O, n48.I)
            wire(n39.valid_down, n48.valid_up)
            n55 = DefineMap_T(16, 0, DefineMap_S(1, DefineRemove_1_S(DefineSTupleToSSeq(ST_Int(), 3, has_valid=True),True),True))()
            wire(n48.O, n55.I)
            wire(n48.valid_down, n55.valid_up)
            n56 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleCreator(ST_SSeq(3, ST_Int()), has_valid=True),True))()
            wire(n29.O, n56.I0)
            wire(n55.O, n56.I1)
            wire(n29.valid_down & n55.valid_down, n56.valid_up)
            n63 = DefineShift_T(16, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n1.O, n63.I)
            wire(n1.valid_down, n63.valid_up)
            n64 = DefineShift_T(16, 0, 1, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n63.O, n64.I)
            wire(n63.valid_down, n64.valid_up)
            n65 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleCreator(ST_Int(), has_valid=True),True))()
            wire(n64.O, n65.I0)
            wire(n63.O, n65.I1)
            wire(n64.valid_down & n63.valid_down, n65.valid_up)
            n72 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True),True))()
            wire(n65.O, n72.I0)
            wire(n1.O, n72.I1)
            wire(n65.valid_down & n1.valid_down, n72.valid_up)
            n81 = DefineMap_T(16, 0, DefinePartition_S(1, 1, ST_SSeq_Tuple(3, ST_Int()), has_valid=True))()
            wire(n72.O, n81.I)
            wire(n72.valid_down, n81.valid_up)
            n88 = DefineMap_T(16, 0, DefineMap_S(1, DefineRemove_1_S(DefineSTupleToSSeq(ST_Int(), 3, has_valid=True),True),True))()
            wire(n81.O, n88.I)
            wire(n81.valid_down, n88.valid_up)
            n89 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleAppender(ST_SSeq(3, ST_Int()), 2, has_valid=True),True))()
            wire(n56.O, n89.I0)
            wire(n88.O, n89.I1)
            wire(n56.valid_down & n88.valid_down, n89.valid_up)
            n96 = DefineReshape_ST(ST_TSeq(16, 0, ST_SSeq(1, ST_SSeq_Tuple(3, ST_SSeq(3, ST_Int())))), ST_TSeq(16, 0, ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq_Tuple(3, ST_SSeq(3, ST_Int()))))), has_valid=True)()
            wire(n89.O, n96.I)
            wire(n89.valid_down, n96.valid_up)
            n99 = DefineMap_T(16, 0, DefineReshape_ST(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq_Tuple(3, ST_SSeq(3, ST_Int())))), ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int()))), has_valid=True))()
            wire(n96.O, n99.I)
            wire(n96.valid_down, n99.valid_up)
            n152 = DefineMap_T(16, 0, Module_0())()
            wire(cls.mod0in, n99.O)
            wire(cls.mod0mul, n152.mod0mul)
            wire(cls.mod0reshape, n152.mod0reshape)
            wire(cls.mod0reduce0out, n152.mod0reduce0out)
            wire(cls.mod0reduce1out, n152.mod0reduce1out)
            wire(cls.mod0reduce2out, n152.mod0reduce2out)
            wire(cls.mod0reduce2valid_up, n152.mod0reduce2valid_up)
            wire(n99.O, n152.I)
            wire(n99.valid_down, n152.valid_up)
            n153 = DefineReshape_ST(ST_TSeq(16, 0, ST_TSeq(1, 0, ST_SSeq(1, ST_Int()))), ST_TSeq(16, 0, ST_SSeq(1, ST_Int())), has_valid=True)()
            wire(n152.O, n153.I)
            wire(n152.valid_down, n153.valid_up)
            n154 = DefineFIFO(ST_TSeq(16, 0, ST_SSeq(1, ST_Int())), 1, has_valid=True)()
            wire(n153.O, n154.I)
            wire(n153.valid_down, n154.valid_up)
            n155 = DefineFIFO(ST_TSeq(16, 0, ST_SSeq(1, ST_Int())), 1, has_valid=True)()
            wire(n154.O, n155.I)
            wire(n154.valid_down, n155.valid_up)
            n156 = DefineFIFO(ST_TSeq(16, 0, ST_SSeq(1, ST_Int())), 1, has_valid=True)()
            wire(n155.O, n156.I)
            wire(n155.valid_down, n156.valid_up)
            wire(n156.O, cls.O)
            wire(n156.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = json.load(open("/tmp/ae_input_02445-64.json"))
fault_inputs0_valid = json.load(open("/tmp/ae_in_valid_02445-65.json"))
fault_output = json.load(open("/tmp/ae_output2445-66.json"))
fault_output_valid = json.load(open("/tmp/ae_out_valid2445-67.json"))
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(16 + 7):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 16 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.I, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(16, 0, ST_SSeq(1, ST_Int()))), 0)
            tester.print("I: ")
            fault_helpers.print_nested_port(tester, tester.circuit.I, num_nested_space_layers(ST_TSeq(16, 0, ST_SSeq(1, ST_Int()))))
            tester.print("\n")
        tester.eval()
        if f_clk > 7:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(16, 0, ST_SSeq(1, ST_Int()))))
        tester.print("\n")
        tester.print("mod0in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod0in, num_nested_space_layers(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int())))))
        tester.print("\n")
        tester.print("mod0mul: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod0mul, num_nested_space_layers(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(3, ST_Int())))))
        tester.print("\n")
        tester.print("mod0reshape: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod0reshape, num_nested_space_layers(ST_TSeq(1, 0, ST_SSeq(1, ST_Int()))))
        tester.print("\n")
        tester.print("mod0reduce0: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod0reduce0out, num_nested_space_layers(ST_TSeq(1, 0, ST_SSeq(3, ST_SSeq(1, ST_Int())))))
        tester.print("\n")
        tester.print("mod0reduce1: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod0reduce1out, num_nested_space_layers(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int())))))
        tester.print("\n")
        tester.print("mod0reduce2: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod0reduce2out, num_nested_space_layers(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq(1, ST_Int())))))
        tester.print("\n")
        tester.print("mod0reduce2valid_up: %d\n", tester.circuit.mod0reduce2valid_up)
        if f_clk >= 7:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 7 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(16, 0, ST_SSeq(1, ST_Int()))), 0)
        tester.step(2)
    #tester.circuit.valid_down.expect(0)
    fault_helpers.compile_and_run(tester)
