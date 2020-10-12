from acab.abstract.core.type_base import TypeInstance
from acab.config import AcabConfig

util = AcabConfig.Get()
DECIMAL_S = util("Module.Numbers", "DECIMAL_S")


FLOAT_t = TypeInstance(path=["number", "float"], primitive=True)
INT_t = TypeInstance(path=["number", "int"], primitive=True)
FRACT_t = TypeInstance(path=["number", "fract"], primitive=True)


def _wrap_float(value, opts=None):
    return str(value).replace('.', DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)
