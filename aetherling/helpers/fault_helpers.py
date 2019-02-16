def print_start_clock(tester, ignored):
    tester.print(ignored, "{")

def print_end_clock(tester, ignored):
    tester.print(ignored, "}, ")

def print_nd_bit_array_port(tester, port, ignored, name = None):
    """
    Iterate an n-dimensional array and print its contents using my custom fault
    :param tester: the tester to print on
    :param port: the port to print
    """
    if name is not None:
        tester.print(ignored, f'\\"{name}\\": ')
    if hasattr(port, "N"):
        for i in range(len(port)):
            if i == 0:
                tester.print(ignored, "[")
            print_nd_bit_array_port(tester, port[i], ignored)
            if i == len(port) - 1:
                tester.print(ignored, "], ")
    else:
        tester.print(port, f"%d, ")

def print_nd_int_array_port(tester, port, ignored, name = None):
    """
    Iterate an n-dimensional array and print its contents using my custom fault
    :param tester: the tester to print on
    :param port: the port to print
    """
    if name is not None:
        tester.print(ignored, f'\\"{name}\\": ')
    if hasattr(port[0], "N"):
        for i in range(len(port)):
            if i == 0:
                tester.print(ignored, "[")
            print_nd_int_array_port(tester, port[i], ignored)
            if i == len(port) - 1:
                tester.print(ignored, "], ")
    else:
        tester.print(port, f"%d, ")

