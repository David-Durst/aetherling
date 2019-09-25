"""
This package provides the space-time types and type-manipulating operators for compiling
from Aetherling's space-time DSL to Magma operators in the aetherling.modules package.

The modules and types in this package do not create any hardware. They are just type manipulations
to simplify calling the real Magma modules in aetherling.modules.
"""
from .space_time_types import *
from .nested_counters import DefineNestedCounters
from .ram_st import DefineRAM_ST
from aetherling.space_time.modules.tuple import *
from .modules import *
from .type_helpers import *
# can't export this as permutation.__init__.py imports files from
# this module and reshape_st imports from permtuation, creating a loop
#from .reshape_st import DefineReshape_ST
