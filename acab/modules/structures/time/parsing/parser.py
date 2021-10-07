""""
A PyParsing parser to create patterns
"""
from fractions import Fraction
import pyparsing as pp
import logging as root_logger

from acab.core.parsing import consts as PC
from acab.core.parsing.consts import QUERY_SYMBOL, N, NG, OPAR, CPAR
from acab.modules.structures.time.pattern_constructor import CTOR_ACT
from acab.modules.structures.time.pattern_constructor import construct_pattern_simple
from acab.modules.structures.time.util import BIND_S, TYPE_INSTANCE_S, VALUE_S, NAME_S, OPT_S, PATTERN_S

from acab.core.parsing import parsers as PU

logging = root_logger.getLogger(__name__)


def make_valbind(s, loc, tokens):
    # TODO: replace this with an acab value
    data = {BIND_S: False,
            OPT_S: False,
            TYPE_INSTANCE_S: NAME_S}
    value = None
    if BIND_S in tokens:
        value = tokens[BIND_S][1]
        data[BIND_S] = True
    elif VALUE_S in tokens:
        value = tokens[VALUE_S][1]
        data[TYPE_INSTANCE_S] = tokens[VALUE_S][0]

    if OPT_S in tokens:
        data[OPT_S] = True
    return (value, data)


# Base Syntax
QUESTION = PC.QUERY_SYMBOL.setResultsName(OPT_S)
TILDE = PC.TILDE
COMMA = PC.COMMA
OBRACKET = PC.OBRACKET
CBRACKET = PC.CBRACKET
LESS = PC.LESS
MORE = PC.MORE

acts = pp.Or([COMMA, PC.COLON, TILDE])

# TO BE HOT LOADED:
HOTLOAD_VALUE = pp.Forward()
HOTLOAD_BIND = pp.Forward()

Time_VALBIND = pp.Or([N(VALUE_S, HOTLOAD_VALUE),
                      PU.N(BIND_S, HOTLOAD_BIND)]) + PU.op(QUESTION)

pattern_contents = pp.OneOrMore(pp.Or([Time_VALBIND, acts]))

pattern = pp.Forward()
openers = pp.Or([OPAR, OBRACKET, LESS])
closers = pp.Or([CPAR, CBRACKET, MORE])

pattern <<= pp.Group(openers + pp.ZeroOrMore(pattern_contents | pattern)
                     + closers + PU.op(QUESTION))

main_pattern = PU.s(OBRACKET) + pattern + PU.s(CBRACKET)

# Actions
Time_VALBIND.setParseAction(make_valbind)
pattern.setParseAction(construct_pattern_simple)
main_pattern.setParseAction(lambda s, l, t: (PATTERN_S, t[0][0]))
OBRACKET.setParseAction(lambda x: CTOR_ACT.PSTART)
CBRACKET.setParseAction(lambda x: CTOR_ACT.PEND)
COMMA.setParseAction(lambda x: CTOR_ACT.PDUAL)
QUESTION.setParseAction(lambda x: CTOR_ACT.OP)
LESS.setParseAction(lambda x: CTOR_ACT.CSTART)
MORE.setParseAction(lambda x: CTOR_ACT.CEND)
TILDE.setParseAction(lambda x: (CTOR_ACT.SIL, {}))

# NAMING
acts.setName("TimePatternActs")
pattern_contents.setName("TimePatternContents")
pattern.setName("TimePattern")
openers.setName("TimeOpeners")
closers.setName("TimeClosers")

# parse_point = main_pattern.ignore(PU.COMMENT)
parse_point = main_pattern

def parseString(s):
    """ The primary access point """
    return parse_point.parseString(s)[0][1]
