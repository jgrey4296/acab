from acab.abstract.core.sentence import Sentence
from acab.config import AcabConfig

util = AcabConfig.Get()
DECIMAL_SYMBOL_S = util("Module.Numbers", "DECIMAL_SYMBOL_S")

# TODO replace this with type_system registration
FLOAT_t = Sentence.build(["number", "float"])
INT_t = Sentence.build(["number", "int"])
FRACT_t = Sentence.build(["number", "fract"])


def _wrap_float(value, opts=None):
    return str(value).replace('.', DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)
