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

@dataclass(frozen=True)
class ST_TSeq():
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

    def magma_repr(self):
        return self.t.magma_repr()

@dataclass(frozen=True)
class ST_SSeq():
    n: int
    t: Kind

    def length(self):
        return self.n * self.t.length()

    def port_width(self):
        return self.n * self.t.port_width()

    def time(self):
        return self.t.time()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

@dataclass(frozen=True)
class ST_SSeq_Tuple():
    n: int
    t: Kind

    def length(self):
        return self.n * self.t.length()

    def port_width(self):
        return self.n * self.t.port_width()

    def time(self):
        return self.t.time()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

@dataclass(frozen=True)
class ST_Atom_Tuple():
    t0: Kind
    t1: Kind

    def length(self):
        return self.t0.length() + self.t1.length()

    def port_width(self):
        return 1

    def time(self):
        return 1

    def magma_repr(self):
        return Tuple(self.t0.magma_repr(), self.t1.magma_repr())

int_width = 8
@dataclass(frozen=True)
class ST_Int():
    def length(self):
        return 8

    def port_width(self):
        return 1

    def time(self):
        return 1

    def magma_repr(self):
        return Array[int_width, Bit]

@dataclass(frozen=True)
class ST_Bit():
    def length(self):
        return 1

    def port_width(self):
        return 1

    def time(self):
        return 1

    def magma_repr(self):
        return Bit

def throughput(t):
    return t.length() / t.time()
