import os

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

