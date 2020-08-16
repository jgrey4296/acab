from acab.abstract.parsing import util as PU
from acab import util
from acab.modules.values.numbers import util as NU
from fractions import Fraction
import logging as root_logger
import pyparsing as pp

logging = root_logger.getLogger(__name__)

def construct_num(toks):
    num = toks['num']
    mul = 1
    if 'neg' in toks:
        mul = -1

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

INT.setParseAction(lambda toks: (NU.INT, int(toks[0])))
DEC.setParseAction(lambda toks: (NU.FLOAT, float("{}.{}".format(toks[0][1],toks[1][1]))))
FRACT.setParseAction(lambda toks: (NU.FRACT, Fraction(toks[0][1], toks[1][1])))

NUM = pp.Or([FRACT, DEC, INT]).setResultsName("num")
NEG_NUM = PU.op(NEG) + NUM

NEG_NUM.setParseAction(construct_num)

def parseString(s):
    return NUM.parseString(s)[0][1]
