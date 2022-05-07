from acab.core.value.sentence import Sentence
from acab.core.config.config import AcabConfig

util = AcabConfig()
DECIMAL = util.value("Module.Numbers", "DECIMAL")

# TODO use config primitives
FLOAT_t = Sentence() << ["number", "float"]
INT_t   = Sentence() << ["number", "int"]
FRACT_t = Sentence() << ["number", "fract"]


def _wrap_float(value, opts=None):
    return str(value).replace('.', DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)
