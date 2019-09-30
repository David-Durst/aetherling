import os
import magma

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

def wire_nested_port(tester, fault_port, value, nesting_layers, cur_idx):
    if nesting_layers > 0:
        for sub_port in fault_port:
            cur_idx = wire_nested_port(tester, sub_port, value, nesting_layers - 1, cur_idx)
        return cur_idx
    else:
        if type(value) is list:
            tester.poke(fault_port.port, value[cur_idx])
        else:
            tester.poke(fault_port.port, value)
        return cur_idx + 1

def expect_nested_port(tester, fault_port, value, nesting_layers, cur_idx):
    if nesting_layers > 0:
        for sub_port in fault_port:
            cur_idx = expect_nested_port(tester, sub_port, value, nesting_layers - 1, cur_idx)
        return cur_idx
    else:
        if type(value) is list:
            fault_port.expect(value[cur_idx])
        else:
            fault_port.expect(value)
        return cur_idx + 1


def compile(testcircuit, name=None):
    circuit_name = name if name is not None else testcircuit.name
    magma.compile("vBuild/" + circuit_name, testcircuit, output="coreir-verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])


def compile_and_run(tester):
    tester.compile_and_run(target="verilator", magma_opts={
        "verilator_debug": True,
        "passes": ["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "verifyconnectivity --noclkrst", "deletedeadinstances"],
        "namespaces": ["aetherlinglib", "commonlib", "mantle", "coreir", "global"]
    }, directory="vBuild/", flags=["-Wno-fatal"])
