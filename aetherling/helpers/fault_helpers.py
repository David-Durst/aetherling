import os
import magma
from magma import *
from magma.circuit import DefineCircuitKind

@cache_definition
def wrap_module_with_top(opDef: DefineCircuitKind) -> DefineCircuitKind:
    class _Wrap(Circuit):
        name = "top"
        IO = opDef.IO.Decl
        name = "top"

        @classmethod
        def definition(cls):
            op_instance = opDef()
            port_names = opDef.interface.ports.keys()
            for port_name in port_names:
                wire(getattr(cls, port_name), getattr(op_instance, port_name))

    return _Wrap

def get_fault_log(callee_file, circuit_name):
    dirname = os.path.dirname(callee_file)
    return f"{dirname}/vBuild/obj_dir/{circuit_name}.log"

def print_start_clock(tester):
    tester.print("{")

def print_end_clock(tester):
    tester.print("}, ")

def print_nd_bit_array_port(tester, port, name = None):
    """
    Iterate an n-dimensional array and print its contents using my custom fault
    :param tester: the tester to print on
    :param port: the port to print
    """
    if name is not None:
        tester.print(f'\\"{name}\\": ')
    if hasattr(port, "N"):
        for i in range(len(port)):
            if i == 0:
                tester.print("[")
            print_nd_bit_array_port(tester, port[i])
            if i == len(port) - 1:
                tester.print("], ")
    else:
        tester.print(f"%d, ", port)

def print_nd_int_array_port(tester, port, name = None):
    """
    Iterate an n-dimensional array and print its contents using my custom fault
    :param tester: the tester to print on
    :param port: the port to print
    """
    if name is not None:
        tester.print(f'\\"{name}\\": ')
    if hasattr(port[0], "N"):
        for i in range(len(port)):
            if i == 0:
                tester.print("[")
            print_nd_int_array_port(tester, port[i])
            if i == len(port) - 1:
                tester.print("], ")
    else:
        tester.print(f"%d, ", port)

def print_nested_port(tester, fault_port, nesting_layers):
    if nesting_layers > 0:
        tester.print("[")
        for sub_port in fault_port:
            print_nested_port(tester, sub_port, nesting_layers - 1)
        tester.print("]")
    else:
        tester.print(f"%d, ", fault_port)

def set_nested_port(tester, fault_port, value, nesting_layers, cur_idx):
    if nesting_layers > 0:
        for sub_port in fault_port:
            cur_idx = set_nested_port(tester, sub_port, value, nesting_layers - 1, cur_idx)
        return cur_idx
    else:
        if type(value) is list:
            tester.poke(fault_port.port, value[cur_idx])
        else:
            tester.poke(fault_port.port, value)
        return cur_idx + 1

int_to_ignore = 253
def expect_nested_port(tester, fault_port, value, nesting_layers, cur_idx):
    if nesting_layers > 0:
        for sub_port in fault_port:
            cur_idx = expect_nested_port(tester, sub_port, value, nesting_layers - 1, cur_idx)
        return cur_idx
    else:
        if type(value) is list:
            if value[cur_idx] != int_to_ignore:
                fault_port.expect(value[cur_idx])
        else:
            if value != int_to_ignore:
                fault_port.expect(value)
        return cur_idx + 1


def compile(testcircuit, name=None):
    circuit_name = name if name is not None else testcircuit.name
    magma.compile("vBuild/" + circuit_name, testcircuit, output="coreir-verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])


def compile_and_run(tester):
    tester.compile_and_run(target="verilator", magma_opts={
        "passes": ["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst"],
        "namespaces": ["aetherlinglib", "commonlib", "mantle", "coreir", "global"]
     # }, directory="vBuild/", flags=["-Wno-fatal",  "--no-decoration", "-O3"])
    }, directory="vBuild/",
       flags=["-Wno-fatal", "--compiler", "clang", "--no-decoration", "--output-split", "20000",
         "--output-split-ctrace", "10000", "-O3"])


def compile_and_run_verilog(tester):
    tester.compile_and_run(magma_output="verilog", skip_compile=True, target="verilator", magma_opts={
        "verilator_debug": True,
        "passes": ["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
        "namespaces": ["aetherlinglib", "commonlib", "mantle", "coreir", "global"]
    }, directory="vBuild/", flags=["-Wno-fatal", "--trace"])
