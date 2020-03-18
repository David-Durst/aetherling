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
        IO = ['I', In(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int(8, False))).magma_repr()),'O', Out(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int(8, False)))]
        st_out_t = ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))
        IO += ['red_1_in', Out(ST_Int(8, False).magma_repr())]
        IO += ['red_1_out', Out(ST_Int(8, False).magma_repr())]
        IO += ['red_1_valid_in', Out(ST_Bit().magma_repr())]
        IO += ['red_1_valid_out', Out(ST_Bit().magma_repr())]
        binary_op = False
        @classmethod
        def definition(cls):
            n146 = DefineConst(ST_TSeq(3, 0, ST_TSeq(3, 0, ST_Int(8, False))), (0,1,0,1,2,1,0,1,0,), has_valid=True, delay=3)()
            wire(cls.valid_up, n146.valid_up)
            n105 = DefineMap2_T(3, 0, DefineMap2_T(3, 0, DefineAtomTupleCreator(ST_Int(8, False), ST_Int(8, False), has_valid=True)))()
            wire(cls.I, n105.I0)
            wire(n146.O, n105.I1)
            wire(cls.valid_up & n146.valid_down, n105.valid_up)
            n116 = DefineMap_T(3, 0, DefineMap_T(3, 0, DefineLShift_Atom(True)))()
            wire(n105.O, n116.I)
            wire(n105.valid_down, n116.valid_up)
            n121 = DefineMap_T(3, 0, DefineReduce_T(3, 0, DefineAdd_Atom(False)))()
            wire(n116.O, n121.I)
            wire(n116.valid_down, n121.valid_up)
            n126 = DefineReduce_T(3, 0, DefineMap_T(1, 2, DefineAdd_Atom(False)))()
            wire(n121.O, n126.I)
            wire(n121.valid_down, n126.valid_up)
            wire(cls.red_1_in, n121.O)
            wire(cls.red_1_out, n126.O)
            wire(cls.red_1_valid_in[0], n121.valid_down)
            wire(cls.red_1_valid_out[0], n126.valid_down)
            n147 = DefineConst(ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))), (4,undef,undef,undef,undef,undef,undef,undef,undef,), has_valid=True, delay=13)()
            wire(cls.valid_up, n147.valid_up)
            n129 = DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineAtomTupleCreator(ST_Int(8, False), ST_Int(8, False), has_valid=True)))()
            wire(n126.O, n129.I0)
            wire(n147.O, n129.I1)
            wire(n126.valid_down & n147.valid_down, n129.valid_up)
            n140 = DefineMap_T(1, 2, DefineMap_T(1, 2, DefineRShift_Atom(True)))()
            wire(n129.O, n140.I)
            wire(n129.valid_down, n140.valid_up)
            wire(n140.O, cls.O)
            wire(n140.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "top"
        IO = ['I', In(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))).magma_repr()),'O', Out(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        IO += ['mod_0_in', Out(ST_Int(8, False).magma_repr())]
        IO += ['mod_0_out', Out(ST_Int(8, False).magma_repr())]
        IO += ['mod_0_valid_in', Out(ST_Bit().magma_repr())]
        IO += ['mod_0_valid_out', Out(ST_Bit().magma_repr())]
        IO += ['n2_shift_I', Out(ST_Int(8, False).magma_repr())]
        IO += ['n2_shift_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['n65_shift_I', Out(ST_Int(8, False).magma_repr())]
        IO += ['n65_shift_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['n65_valid_down', Out(ST_Bit().magma_repr())]
        IO += ['n66_shift_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['n66_valid_down', Out(ST_Bit().magma_repr())]
        IO += ['ser_91_valid_up', Out(ST_Bit().magma_repr())]
        IO += ['ser_91_I', Out(ST_SSeq(3, ST_Int(8, False)).magma_repr())]
        IO += ['ser_91_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['ser_101_I', Out(ST_SSeq(3, ST_Int(8, False)).magma_repr())]
        IO += ['ser_101_O', Out(ST_Int(8, False).magma_repr())]
        IO += ['red_1_in', Out(ST_Int(8, False).magma_repr())]
        IO += ['red_1_out', Out(ST_Int(8, False).magma_repr())]
        IO += ['red_1_valid_in', Out(ST_Bit().magma_repr())]
        IO += ['red_1_valid_out', Out(ST_Bit().magma_repr())]
        st_in_t = [ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))))]
        st_out_t = ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))))
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))), 1, has_valid=True)()
            wire(cls.I, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n2 = DefineShift_TN(16, (1,1,), 0, (2,2,), 4, ST_Int(8, False), has_valid=True)()
            wire(cls.n2_shift_I, n1.O)
            wire(cls.n2_shift_O, n2.O)
            wire(n1.O, n2.I)
            wire(n1.valid_down, n2.valid_up)
            n3 = DefineShift_TN(16, (1,1,), 0, (2,2,), 4, ST_Int(8, False), has_valid=True)()
            wire(n2.O, n3.I)
            wire(n2.valid_down, n3.valid_up)
            n4 = DefineShift_TN(16, (1,1,), 0, (2,2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            n5 = DefineShift_TN(16, (1,1,), 0, (2,2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n4.O, n5.I)
            wire(n4.valid_down, n5.valid_up)
            n6 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(8, False), has_valid=True))))()
            wire(n5.O, n6.I0)
            wire(n4.O, n6.I1)
            wire(n5.valid_down & n4.valid_down, n6.valid_up)
            n16 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(8, False), 2, has_valid=True))))()
            wire(n6.O, n16.I0)
            wire(n3.O, n16.I1)
            wire(n6.valid_down & n3.valid_down, n16.valid_up)
            n30 = DefineMap_T(16, 0, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int(8, False))))()
            wire(n16.O, n30.I)
            wire(n16.valid_down, n30.valid_up)
            n31 = DefineShift_TN(16, (1,1,), 0, (2,2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n2.O, n31.I)
            wire(n2.valid_down, n31.valid_up)
            n32 = DefineShift_TN(16, (1,1,), 0, (2,2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n31.O, n32.I)
            wire(n31.valid_down, n32.valid_up)
            n33 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(8, False), has_valid=True))))()
            wire(n32.O, n33.I0)
            wire(n31.O, n33.I1)
            wire(n32.valid_down & n31.valid_down, n33.valid_up)
            n43 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(8, False), 2, has_valid=True))))()
            wire(n33.O, n43.I0)
            wire(n2.O, n43.I1)
            wire(n33.valid_down & n2.valid_down, n43.valid_up)
            n57 = DefineMap_T(16, 0, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int(8, False))))()
            wire(n43.O, n57.I)
            wire(n43.valid_down, n57.valid_up)
            n58 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_TSeq(3, 0, ST_Int(8, False)), has_valid=True)))()
            wire(n30.O, n58.I0)
            wire(n57.O, n58.I1)
            wire(n30.valid_down & n57.valid_down, n58.valid_up)
            n65 = DefineShift_TN(16, (1,1,), 0, (2,2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n1.O, n65.I)
            wire(n1.valid_down, n65.valid_up)
            n66 = DefineShift_TN(16, (1,1,), 0, (2,2,), 1, ST_Int(8, False), has_valid=True)()
            wire(n65.O, n66.I)
            wire(cls.n65_shift_I, n1.O)
            wire(cls.n65_shift_O, n65.O)
            wire(cls.n65_valid_down[0], n65.valid_down)
            wire(cls.n66_shift_O, n66.O)
            wire(cls.n66_valid_down[0], n66.valid_down)
            wire(n65.valid_down, n66.valid_up)
            n67 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleCreator(ST_Int(8, False), has_valid=True))))()
            wire(n66.O, n67.I0)
            wire(n65.O, n67.I1)
            wire(n66.valid_down & n65.valid_down, n67.valid_up)
            n77 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_Int(8, False), 2, has_valid=True))))()
            wire(n67.O, n77.I0)
            wire(n1.O, n77.I1)
            wire(n67.valid_down & n1.valid_down, n77.valid_up)
            n91 = DefineMap_T(16, 0, DefineMap_T(1, 2, DefineSerialize(3, 0, ST_Int(8, False))))()
            wire(n77.O, n91.I)
            wire(cls.ser_91_valid_up[0], n77.valid_down)
            wire(cls.ser_91_I, n77.O)
            wire(cls.ser_91_O, n91.O)
            wire(n77.valid_down, n91.valid_up)
            n92 = DefineMap2_T(16, 0, DefineMap2_T(1, 2, DefineSSeqTupleAppender(ST_TSeq(3, 0, ST_Int(8, False)), 2, has_valid=True)))()
            wire(n58.O, n92.I0)
            wire(n91.O, n92.I1)
            wire(n58.valid_down & n91.valid_down, n92.valid_up)
            n101 = DefineMap_T(16, 0, DefineSerialize(3, 0, ST_TSeq(3, 0, ST_Int(8, False))))()
            wire(n92.O, n101.I)
            wire(cls.ser_101_I, n92.O)
            wire(cls.ser_101_O, n101.O)
            wire(n92.valid_down, n101.valid_up)
            n141 = DefineMap_T(16, 0, Module_0())()
            wire(cls.mod_0_in, n101.O)
            wire(cls.mod_0_out, n141.O)
            wire(cls.mod_0_valid_in[0], n101.valid_down)
            wire(cls.mod_0_valid_out[0], n141.valid_down)
            wire(cls.red_1_in, n141.red_1_in)
            wire(cls.red_1_out, n141.red_1_out)
            wire(cls.red_1_valid_in, n141.red_1_valid_in)
            wire(cls.red_1_valid_out, n141.red_1_valid_out)
            wire(n101.O, n141.I)
            wire(n101.valid_down, n141.valid_up)
            n142 = DefineReshape_ST(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))), ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))), has_valid=True)()
            wire(n141.O, n142.I)
            wire(n141.valid_down, n142.valid_up)
            n143 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))), 1, has_valid=True)()
            wire(n142.O, n143.I)
            wire(n142.valid_down, n143.valid_up)
            n144 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))), 1, has_valid=True)()
            wire(n143.O, n144.I)
            wire(n143.valid_down, n144.valid_up)
            n145 = DefineFIFO(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False)))), 1, has_valid=True)()
            wire(n144.O, n145.I)
            wire(n144.valid_down, n145.valid_up)
            wire(n145.O, cls.O)
            wire(n145.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = json.load(open("/tmp/ae_input_05819-0.json"))
fault_inputs0_valid = json.load(open("/tmp/ae_in_valid_05819-1.json"))
fault_output = json.load(open("/tmp/ae_output5819-2.json"))
fault_output_valid = json.load(open("/tmp/ae_out_valid5819-3.json"))
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(144 + 16):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 144 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.I, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))))), 0)
            tester.print("I: ")
            fault_helpers.print_nested_port(tester, tester.circuit.I, num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))))))
            tester.print("\n")
        tester.eval()
        if f_clk > 16:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))))))
        tester.print("\n")
        tester.print("mod_0_in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod_0_in, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("mod_0_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod_0_out, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("red_1_in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.red_1_in, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("red_1_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.red_1_out, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("red_1_valid_in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.red_1_valid_in, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("red_1_valid_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.red_1_valid_out, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("\n")
        tester.print("mod_0_valid_in: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod_0_valid_in, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("mod_0_valid_out: ")
        fault_helpers.print_nested_port(tester, tester.circuit.mod_0_valid_out, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("\n")
        """  
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
        tester.print("n65_shift_I: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n65_shift_I, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("n65_shift_O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n65_shift_O, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("n65_valid_down: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n65_valid_down, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("\n")
        tester.print("n66_shift_O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n66_shift_O, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("n66_valid_down: ")
        fault_helpers.print_nested_port(tester, tester.circuit.n66_valid_down, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("\n")
        tester.print("ser_91_valid_up: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_91_valid_up, num_nested_space_layers(
            ST_Bit()
        ))
        tester.print("\n")
        tester.print("ser_91_I: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_91_I, num_nested_space_layers(
            ST_SSeq(3, ST_Int(8, False))
        ))
        tester.print("\n")
        tester.print("ser_91_O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_91_O, num_nested_space_layers(
            ST_Int(8, False)
        ))
        tester.print("\n")
        tester.print("ser_101_I: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_101_I, num_nested_space_layers(
            ST_SSeq(3, ST_Int(8, False))
        ))
        tester.print("\n")
        tester.print("ser_101_O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.ser_101_O, num_nested_space_layers(
            ST_Int(8, False)
        ))
        """
        tester.print("\n")
        if f_clk >= 16:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 16 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(16, 0, ST_TSeq(1, 2, ST_TSeq(1, 2, ST_Int(8, False))))), 0)
        tester.step(2)
    tester.circuit.valid_down.expect(0)
    fault_helpers.compile_and_run(tester)
