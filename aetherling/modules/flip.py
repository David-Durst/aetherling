from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from magma import *
from magma.circuit import DefineCircuitKind
from .hydrate import Dehydrate, Hydrate
from .map_fully_parallel_sequential import MapParallel
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface


