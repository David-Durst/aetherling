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
        IO = ['I', In(ST_TSeq(1, 0, ST_SSeq(3, ST_TSeq(3, 0, ST_Int(8, False)))).magma_repr()),'O', Out(ST_TSeq(1, 0, ST_TSeq(1, 2, ST_Int(8, False))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(1, 0, ST_SSeq(3, ST_TSeq(3, 0, ST_Int(8, False))))]
        st_out_t = ST_TSeq(1, 0, ST_TSeq(1, 2, ST_Int(8, False)))
        binary_op = False
        @classmethod
        def definition(cls):
            n145 = DefineConst(ST_TSeq(1, 0, ST_SSeq(3, ST_TSeq(3, 0, ST_Int(8, False)))), ((0,1,0,),(1,2,1,),(0,1,0,),), has_valid=True, delay=2)()
            wire(cls.valid_up, n145.valid_up)
            n91 = DefineMap2_T(1, 0, DefineMap2_S(3, DefineMap2_T(3, 0, DefineAtomTupleCreator(ST_Int(8, False), ST_Int(8, False), has_valid=True)),True))()
            wire(cls.I, n91.I0)
            wire(n145.O, n91.I1)
            wire(cls.valid_up & n145.valid_down, n91.valid_up)
            n107 = DefineMap_T(1, 0, DefineMap_S(3, DefineMap_T(3, 0, DefineLShift_Atom(True)),True))()
            wire(n91.O, n107.I)
            wire(n91.valid_down, n107.valid_up)
            n114 = DefineMap_T(1, 0, DefineMap_S(3, DefineReduce_T(3, 0, DefineAdd_Atom(False)),True))()
            wire(n107.O, n114.I)
            wire(n107.valid_down, n114.valid_up)
            n121 = DefineMap_T(1, 0, DefineReduce_S(3, DefineMap_T(1, 2, DefineAdd_Atom(False)), has_valid=True))()
            wire(n114.O, n121.I)
            wire(n114.valid_down, n121.valid_up)
            n124 = DefineReduce_T(1, 0, DefineMap_S(1, DefineMap_T(1, 2, DefineAdd_Atom(False)),False))()
            wire(n121.O, n124.I)
            wire(n121.valid_down, n124.valid_up)
            n125 = DefineReshape_ST(ST_TSeq(1, 0, ST_SSeq(1, ST_TSeq(1, 2, ST_Int(8, False)))), ST_TSeq(1, 0, ST_TSeq(1, 2, ST_Int(8, False))), has_valid=True)()
            wire(n124.O, n125.I)
            wire(n124.valid_down, n125.valid_up)
            n146 = DefineConst(ST_TSeq(1, 0, ST_TSeq(1, 2, ST_Int(8, False))), (4,undef,undef,), has_valid=True, delay=7)()
            wire(cls.valid_up, n146.valid_up)
            n128 = DefineMap2_T(1, 0, DefineMap2_T(1, 2, DefineAtomTupleCreator(ST_Int(8, False), ST_Int(8, False), has_valid=True)))()
            wire(n125.O, n128.I0)
            wire(n146.O, n128.I1)
            wire(n125.valid_down & n146.valid_down, n128.valid_up)
            n139 = DefineMap_T(1, 0, DefineMap_T(1, 2, DefineRShift_Atom(True)))()
            wire(n128.O, n139.I)
            wire(n128.valid_down, n139.valid_up)
            wire(n139.O, cls.O)
            wire(n139.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "top"
        IO = ['I', In(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))).magma_repr()),'O', Out(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        IO += ['mod_0_in', Out(ST_TSeq(1, 0, ST_SSeq(3, ST_TSeq(3, 0, ST_Int(8, False)))).magma_repr())]
        IO += ['n2_shift_I', Out(ST_Int(8, False).magma_repr())]
        IO += ['n2_shift_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['n55_shift_I', Out(ST_Int(8, False).magma_repr())]
        IO += ['n55_shift_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['ser_0_in', Out(ST_SSeq(1, ST_SSeq(3, ST_Int(8, False))).magma_repr())]
        IO += ['ser_0_out', Out(ST_SSeq(1, ST_Int(8, False)).magma_repr())]
        st_in_t = [ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False)))]
        st_out_t = ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False)))
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))), 1, has_valid=True)()
            wire(cls.I, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_TN(16, (1,), 0, (2,), 4, ST_Int(8, False), has_valid=True)()
            wire(n1.O, n2.I)
            wire(cls.n2_shift_I, n1.O)
            wire(cls.n2_shift_O, n2.O)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_TN(16, (1,), 0, (2,), 4, ST_Int(8, False), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineShift_TN(16, (1,), 0, (2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            n5 = DefineShift_TN(16, (1,), 0, (2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n4.O, n5.I)
            wire(n4.valid_down, n5.valid_up)
            n6 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(8, False), has_valid=True)))()
            wire(n5.O, n6.I0)
            wire(n4.O, n6.I1)
            wire(n5.valid_down & n4.valid_down, n6.valid_up)
            n13 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(8, False), 2, has_valid=True)))()
            wire(n6.O, n13.I0)
            wire(n3.O, n13.I1)
            wire(n6.valid_down & n3.valid_down, n13.valid_up)
            n20 = DefineReshape_ST(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_SSeq_Tuple(3, ST_Int(8, False)))), ST_TSeq(16, 0, ST_SSeq(1, ST_TSeq(1, 2, ST_SSeq_Tuple(3, ST_Int(8, False))))), has_valid=True)()
            wire(n13.O, n20.I)
            wire(n13.valid_down, n20.valid_up)
            n25 = DefineMap_T(16, 0, DefineMap_S(1, DefineSerialize(3, 0, ST_Int(8, False)),True))()
            wire(cls.ser_0_in, n20.O)
            wire(cls.ser_0_out, n25.O)
            wire(n20.O, n25.I)
            wire(n20.valid_down, n25.valid_up)
            n26 = DefineShift_TN(16, (1,), 0, (2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n2.O, n26.I)
            wire(n2.valid_down, n26.valid_up)
            n27 = DefineShift_TN(16, (1,), 0, (2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n26.O, n27.I)
            wire(n26.valid_down, n27.valid_up)
            n28 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(8, False), has_valid=True)))()
            wire(n27.O, n28.I0)
            wire(n26.O, n28.I1)
            wire(n27.valid_down & n26.valid_down, n28.valid_up)
            n35 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(8, False), 2, has_valid=True)))()
            wire(n28.O, n35.I0)
            wire(n2.O, n35.I1)
            wire(n28.valid_down & n2.valid_down, n35.valid_up)
            n42 = DefineReshape_ST(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_SSeq_Tuple(3, ST_Int(8, False)))), ST_TSeq(16, 0, ST_SSeq(1, ST_TSeq(1, 2, ST_SSeq_Tuple(3, ST_Int(8, False))))), has_valid=True)()
            wire(n35.O, n42.I)
            wire(n35.valid_down, n42.valid_up)
            n47 = DefineMap_T(16, 0, DefineMap_S(1, DefineSerialize(3, 0, ST_Int(8, False)),True))()
            wire(n42.O, n47.I)
            wire(n42.valid_down, n47.valid_up)
            n48 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleCreator(ST_TSeq(3, 0, ST_Int(8, False)), has_valid=True),True))()
            wire(n25.O, n48.I0)
            wire(n47.O, n48.I1)
            wire(n25.valid_down & n47.valid_down, n48.valid_up)
            n55 = DefineShift_TN(16, (1,), 0, (2,), 1, ST_Int(8, False), has_valid=True)()
            wire(cls.n55_shift_I, n1.O)
            wire(cls.n55_shift_O, n55.O)
            wire(n1.O, n55.I)
            wire(n1.valid_down, n55.valid_up)
            n56 = DefineShift_TN(16, (1,), 0, (2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n55.O, n56.I)
            wire(n55.valid_down, n56.valid_up)
            n57 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(8, False), has_valid=True)))()
            wire(n56.O, n57.I0)
            wire(n55.O, n57.I1)
            wire(n56.valid_down & n55.valid_down, n57.valid_up)
            n64 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(8, False), 2, has_valid=True)))()
            wire(n57.O, n64.I0)
            wire(n1.O, n64.I1)
            wire(n57.valid_down & n1.valid_down, n64.valid_up)
            n71 = DefineReshape_ST(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_SSeq_Tuple(3, ST_Int(8, False)))), ST_TSeq(16, 0, ST_SSeq(1, ST_TSeq(1, 2, ST_SSeq_Tuple(3, ST_Int(8, False))))), has_valid=True)()
            wire(n64.O, n71.I)
            wire(n64.valid_down, n71.valid_up)
            n76 = DefineMap_T(16, 0, DefineMap_S(1, DefineSerialize(3, 0, ST_Int(8, False)),True))()
            wire(n71.O, n76.I)
            wire(n71.valid_down, n76.valid_up)
            n77 = DefineMap2_T(16, 0, DefineMap2_S(1, DefineSSeqTupleAppender(ST_TSeq(3, 0, ST_Int(8, False)), 2, has_valid=True),True))()
            wire(n48.O, n77.I0)
            wire(n76.O, n77.I1)
            wire(n48.valid_down & n76.valid_down, n77.valid_up)
            n84 = DefineReshape_ST(ST_TSeq(16, 0, ST_SSeq(1, ST_SSeq_Tuple(3, ST_TSeq(3, 0, ST_Int(8, False))))), ST_TSeq(16, 0, ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq_Tuple(3, ST_TSeq(3, 0, ST_Int(8, False)))))), has_valid=True)()
            wire(n77.O, n84.I)
            wire(n77.valid_down, n84.valid_up)
            n87 = DefineMap_T(16, 0, DefineReshape_ST(ST_TSeq(1, 0, ST_SSeq(1, ST_SSeq_Tuple(3, ST_TSeq(3, 0, ST_Int(8, False))))), ST_TSeq(1, 0, ST_SSeq(3, ST_TSeq(3, 0, ST_Int(8, False)))), has_valid=True))()
            wire(n84.O, n87.I)
            wire(n84.valid_down, n87.valid_up)
            wire(cls.mod_0_in, n87.O)
            n140 = DefineMap_T(16, 0, Module_0())()
            wire(n87.O, n140.I)
            wire(n87.valid_down, n140.valid_up)
            n141 = DefineReshape_ST(ST_TSeq(16, 0, ST_TSeq(1, 0, ST_TSeq(1, 2, ST_Int(8, False)))), ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))), has_valid=True)()
            wire(n140.O, n141.I)
            wire(n140.valid_down, n141.valid_up)
            n142 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))), 1, has_valid=True)()
            wire(n141.O, n142.I)
            wire(n141.valid_down, n142.valid_up)
            n143 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))), 1, has_valid=True)()
            wire(n142.O, n143.I)
            wire(n142.valid_down, n143.valid_up)
            n144 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False))), 1, has_valid=True)()
            wire(n143.O, n144.I)
            wire(n143.valid_down, n144.valid_up)
            wire(n144.O, cls.O)
            wire(n144.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = json.load(open("/tmp/ae_input_023569-78.json"))
fault_inputs0_valid = json.load(open("/tmp/ae_in_valid_023569-79.json"))
fault_output = json.load(open("/tmp/ae_output23569-80.json"))
fault_output_valid = json.load(open("/tmp/ae_out_valid23569-81.json"))
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(48 + 10):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 48 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.I, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False)))), 0)
            tester.print("I: ")
            fault_helpers.print_nested_port(tester, tester.circuit.I, num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False)))))
            tester.print("\n")
        tester.eval()
        if f_clk > 10:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False)))))
        tester.print("\n")
        tester.print("mod_0_in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod_0_in, num_nested_space_layers(
            ST_TSeq(1, 0, ST_SSeq(3, ST_TSeq(3, 0, ST_Int(8, False))))
        ))
        tester.print("\n")
        tester.print("n2_shift_I: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n2_shift_I, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("n2_shift_O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n2_shift_O, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("n55_shift_I: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n55_shift_I, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("n55_shift_O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n55_shift_O, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("ser_0_in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_0_in, num_nested_space_layers(
            ST_SSeq(1, ST_SSeq(3, ST_Int(8, False)))
        ))
        tester.print("\n")
        tester.print("ser_0_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_0_out, num_nested_space_layers(
            ST_SSeq(1,ST_Int(8, False))
        ))
        tester.print("\n")
        if f_clk >= 10:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 10 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_Int(8, False)))), 0)
        tester.step(2)
    tester.circuit.valid_down.expect(0)
    fault_helpers.compile_and_run(tester)
