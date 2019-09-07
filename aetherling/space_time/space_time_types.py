"""
The classes for constructing types in Aetherling's space-time IR
"""
from dataclasses import dataclass
from magma import Type, Kind, Array, Bit, Tuple


def is_nested(st_type):
    if type(st_type) in [ST_TSeq, ST_SSeq, ST_SSeq_Tuple]:
        return True
    else:
        return False

class ST_Type():
    def length(self) -> int:
        """
        Total amount of data over the entire time of the ST type
        """
        pass

    def port_width(self) -> int:
        """
        Number of atoms each active clock
        """
        pass

    def time(self) -> int:
        """
        Number of clocks required for an operator to accept or emit this type
        """
        pass

    def valid_clocks(self) -> int:
        """
        Number of valid clocks in .time() clocks
        """
        pass

    def magma_repr(self) -> Kind:
        """
        A magma representation of this type as a nested array of bits.
        Magma doesn't acount for time.
        """
        pass


@dataclass(frozen=True)
class ST_TSeq(ST_Type):
    n: int
    i: int
    t: Kind

    def length(self):
        """
        Total amount of data over the entire time of the ST type
        """
        return self.n * self.t.length()

    def port_width(self):
        """
        Number of atoms each active clock
        """
        return self.t.port_width()

    def time(self):
        return (self.n + self.i) * self.t.time()

    def valid_clocks(self):
        return self.n * self.t.valid_clocks()

    def magma_repr(self):
        return self.t.magma_repr()

    def __str__(self):
        return "TSeq({}, {}, {})".format(str(self.n), str(self.i), str(self.t))

@dataclass(frozen=True)
class ST_SSeq(ST_Type):
    n: int
    t: Kind

    def length(self):
        return self.n * self.t.length()

    def port_width(self):
        return self.n * self.t.port_width()

    def time(self):
        return self.t.time()

    def valid_clocks(self) -> int:
        return self.t.valid_clocks()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

    def __str__(self):
        return "SSeq({}, {})".format(str(self.n), str(self.t))

@dataclass(frozen=True)
class ST_SSeq_Tuple(ST_Type):
    n: int
    t: Kind

    def length(self):
        return self.n * self.t.length()

    def port_width(self):
        return self.n * self.t.port_width()

    def time(self):
        return self.t.time()

    def valid_clocks(self) -> int:
        return self.t.valid_clocks()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

    def __str__(self):
        return "STuple({}, {})".format(str(self.n), str(self.t))

@dataclass(frozen=True)
class ST_Atom_Tuple(ST_Type):
    t0: Kind
    t1: Kind

    def length(self):
        return self.t0.length() + self.t1.length()

    def port_width(self):
        return 1

    def time(self):
        return 1

    def valid_clocks(self) -> int:
        return 1

    def magma_repr(self):
        return Tuple(self.t0.magma_repr(), self.t1.magma_repr())

    def __str__(self):
        return "ATuple({}, {})".format(str(self.t0), str(self.t1))

int_width = 8
@dataclass(frozen=True)
class ST_Int(ST_Type):
    def length(self):
        return 8

    def port_width(self):
        return 1

    def time(self):
        return 1

    def valid_clocks(self) -> int:
        return 1

    def magma_repr(self):
        return Array[int_width, Bit]

    def __str__(self):
        return "Int"

@dataclass(frozen=True)
class ST_Bit(ST_Type):
    def length(self):
        return 1

    def port_width(self):
        return 1

    def time(self):
        return 1

    def valid_clocks(self) -> int:
        return 1

    def magma_repr(self):
        return Bit

    def __str__(self):
        return "Bit"

def throughput(t):
    return t.length() / t.time()
