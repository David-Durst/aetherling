from mantle.common.countermod import DefineCounterModM
from mantle.coreir import DefineCoreirConst
from magma import *
from aetherling.modules.term_any_type import TermAnyType
import math


def AESizedCounterModM(m, cin=False, cout=False, incr=1,
                     has_ce=False, has_reset=False, **kwargs):
    """
    This is that counts from 0 to m - 1 that uses the minimum number of bits
    :param m: The value the counter counts up to
    :param cin: Whether this counter should have a carry input
    :param cout: Whether this counter should a carry output
    :param incr: How much this counter should increment by per clock. Default is 1.
    :param has_ce: Whether this counter should a clock-enable input
    :param kwargs: Args passed to the counter circuit when it is being initialized
    :return: A counter circuit
    """
    if m > 1:
        return DefineCounterModM(m, math.ceil(math.log(m, 2)), cin, cout, incr, has_ce, has_reset)(**kwargs)
    else:
        class _SizedCounter(Circuit):
            name = 'SizedCounter_1_cin{}_cout{}_incr{}_hasCE{}_hasReset{}'.format(
                str(cin), str(cout), str(incr), str(has_ce), str(has_reset))
            IO = ['O', Out(Array[1, Bit])] + ClockInterface(has_ce=has_ce, has_reset=has_reset)
            if cin:
                IO += ['CIN', In(Bit)]
            if cout:
                IO += ["COUT", Out(Bit)]

            @classmethod
            def definition(cls):
                output = array(0, 1)
                wire(output, cls.O)
                if has_ce:
                    wire(cls.CE, TermAnyType(Bit))
                if has_reset:
                    wire(cls.RESET, TermAnyType(Bit))
                if cin:
                    wire(cls.CIN, TermAnyType(Bit))
                if cout:
                    wire(cls.COUT, output[0])
        return _SizedCounter()
