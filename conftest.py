import pytest
from magma import clear_cachedFunctions
from magma.backend.coreir_ import CoreIRContextSingleton
import coreir
import fault


@pytest.fixture(autouse=True)
def mantle_test():
    """
    Clear the circuit cache before running, allows name reuse across tests
    without collisions
    """
    import magma.config
    magma.config.set_compile_dir('callee_file_dir')
    fault.config.set_test_dir('callee_file_dir')
    print('mantle_test_resetting')
    #global magma_coreir_context
    #magma_coreir_context = coreir.Context()
    CoreIRContextSingleton().reset_instance()
    clear_cachedFunctions()
