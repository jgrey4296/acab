from py_rule.abstract.parsing import util as PU
from py_rule import util
import logging as root_logger
import pyparsing as pp

logging = root_logger.getLogger(__name__)


def construct_num(toks):
    # TODO: add in fractions and underscores
    if util.DECIMAL_S in toks[0]:
        return (util.FLOAT_S, float(toks[0].replace(util.DECIMAL_S,
                                                    '.')))
    else:
        return (util.INT_S, int(toks[0]))



NUM = pp.Word(pp.nums + util.SUB_S + util.DECIMAL_S)
NUM.setParseAction(construct_num)

def parseString(s):
    return NUM.parseString(s)[0][1]
