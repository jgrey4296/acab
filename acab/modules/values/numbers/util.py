from acab.abstract.type_base import TypeInstance
from acab.config import AcabConfig
import acab.abstract.printing.util as PrU

util = AcabConfig.Get()
DECIMAL_S = util("Module.Numbers", "DECIMAL_S")


FLOAT_t = TypeInstance(path=["number", "float"], primitive=True)
INT_t = TypeInstance(path=["number", "int"], primitive=True)
FRACT_t = TypeInstance(path=["number", "fract"], primitive=True)


def _wrap_float(value, opts=None):
    return str(value).replace('.', DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)


PrU.register_primitive({FLOAT_t: _wrap_float,
                        INT_t: _wrap_int})
PrU.register_obvious_types(FLOAT_t, INT_t, FRACT_t)

