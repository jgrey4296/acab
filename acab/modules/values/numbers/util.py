from acab.abstract.core.type_base import TypeInstance
from acab.config import AcabConfig

util = AcabConfig.Get()
DECIMAL_SYMBOL_S = util("Module.Numbers", "DECIMAL_SYMBOL_S")

# TODO replace this with type_system registration
FLOAT_t = TypeInstance(path=["number", "float"])
INT_t = TypeInstance(path=["number", "int"])
FRACT_t = TypeInstance(path=["number", "fract"])


def _wrap_float(value, opts=None):
    return str(value).replace('.', DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)
