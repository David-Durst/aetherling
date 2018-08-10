from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.hydrate import Dehydrate
from mantle.coreir.type_helpers import Term


__all__ = ['DefineTermAnyType', 'TermAnyType']

@cache_definition
def DefineTermAnyType(cirb: CoreIRBackend, t: Kind):
    """
    Generate Serial-In, Parallel-Out shift register that handles any type.

    I : In(t),  O : Out(Array(n, t))
    """

    class _SIPO(Circuit):
        name = 'Term_{}t'.format(cleanName(str(t)))
        IO = ['I', In(t)]
        @classmethod
        def definition(cls):
            type_size_in_bits = cirb.get_type(t, True).size
            type_to_bits = Dehydrate(cirb, t)
            term = Term(cirb, type_size_in_bits)
            wire(cls.I, type_to_bits.I)
            wire(type_to_bits.out, term.I)

    return _SIPO

def TermAnyType(cirb: CoreIRBackend, t: Kind):
    return DefineTermAnyType(cirb, t)()
