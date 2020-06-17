from acab.abstract.parsing import util as PU
from acab import util
import logging as root_logger
import pyparsing as pp

logging = root_logger.getLogger(__name__)


def construct_num(toks):
    # TODO: add in fractions and underscores
    as_str = "".join(toks)
    if util.DECIMAL_S in toks[0]:
        return (util.FLOAT_S, float(as_str.replace(util.DECIMAL_S, '.')))
    else:
        return (util.INT_S, int(as_str))

# This avoids trying to parse rebind arrows as numbers
NEG = pp.Keyword("-", identChars=">")

NUM = PU.op(NEG) + pp.Word(pp.nums + util.DECIMAL_S)
NUM.setParseAction(construct_num)

def parseString(s):
    return NUM.parseString(s)[0][1]
