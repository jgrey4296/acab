from acab.abstract.parsing import util as PU
from acab import util
from acab.modules.values.numbers import util as NU
from fractions import Fraction
import logging as root_logger
import pyparsing as pp

logging = root_logger.getLogger(__name__)

def construct_num(toks):
    num = toks[0]
    mul = 1
    if 'neg' in toks:
        mul = -1
        num = toks[1]

    return (num[0], num[1] * mul)

# This avoids trying to parse rebind arrows as numbers
NEG = pp.Group(pp.Keyword("-", identChars=">")).setResultsName("neg")
DECIMAL_MARK = PU.s(pp.Literal(util.DECIMAL_S))

INT = pp.Word(pp.nums)
DEC = INT + DECIMAL_MARK + INT
FRACT = INT + PU.SLASH + INT
# TODO: parse octal and hex numbers?
OCT = pp.empty
HEX = pp.empty

def build_int(toks):
    return (NU.INT_t, int(toks[0]))

def build_decimal(toks):
    the_num = float("{}.{}".format(toks[0][1],toks[1][1]))
    return (NU.FLOAT_t, the_num)

def build_fraction(toks):
    the_num = Fraction(toks[0][1], toks[1][1])
    return (NU.FRACT_t, the_num)


INT.addParseAction(build_int)
DEC.setParseAction(build_decimal)
FRACT.setParseAction(build_fraction)

# INT.addCondition(lambda toks: isinstance(toks[0], tuple))
# DEC.addCondition(lambda toks: isinstance(toks[0], tuple))
# FRACT.addCondition(lambda toks: isinstance(toks[0], tuple))
# NUM = pp.Or([FRACT, DEC, INT]).setResultsName("num")
# NEG_NUM = PU.op(NEG) + NUM
INT.setName("int")
DEC.setName("decimal")
FRACT.setName("fraction")

NEG_NUM = PU.op(NEG) + pp.Or([FRACT, DEC, INT]).setResultsName("num")

# NEG_NUM.addParseAction(lambda toks: breakpoint())
# NEG_NUM.addCondition(lambda toks: isinstance(toks['num'][0], tuple))
NEG_NUM.addParseAction(construct_num)

NEG_NUM.setName("NumP")


def parseString(s):
    return NEG_NUM.parseString(s)[0]
