from acab.core.data.values import Sentence
from acab.core.config.config import AcabConfig

util = AcabConfig.Get()
DECIMAL = util.value("Module.Numbers", "DECIMAL")

# TODO use config primitives
FLOAT_t = Sentence.build(["number", "float"])
INT_t   = Sentence.build(["number", "int"])
FRACT_t = Sentence.build(["number", "fract"])


def _wrap_float(value, opts=None):
    return str(value).replace('.', DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)
