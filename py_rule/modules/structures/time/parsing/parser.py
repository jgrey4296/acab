""""
A PyParsing parser to create patterns
"""
from py_rule.modules.structures.time.pattern_constructor import CTOR_ACT
from py_rule.modules.structures.time.pattern_constructor import construct_pattern_simple
from py_rule.abstract.parsing import util as PU
from py_rule.modules.structures.time.util import BIND_S, VALUE_TYPE_S, VALUE_S, NAME_S, OPT_S, PATTERN_S
from fractions import Fraction
import pyparsing as pp
import logging as root_logger

logging = root_logger.getLogger(__name__)


def make_valbind(tokens):
    # TODO: replace this with a pyrule value
    data = {BIND_S: False,
            OPT_S: False,
            VALUE_TYPE_S: NAME_S}
    value = None
    if BIND_S in tokens:
        value = tokens[BIND_S][1]
        data[BIND_S] = True
    elif VALUE_S in tokens:
        value = tokens[VALUE_S][1]
        data[VALUE_TYPE_S] = tokens[VALUE_S][0]

    if OPT_S in tokens:
        data[OPT_S] = True
    return (value, data)


# Base Syntax
QUESTION = PU.QMARK.setResultsName(OPT_S)
TILDE = PU.TILDE
COMMA = PU.COMMA
OBRACKET = PU.OBRACKET
CBRACKET = PU.CBRACKET
LESS = PU.LESS
MORE = PU.MORE

acts = pp.Or([COMMA, PU.COLON, TILDE])

# TO BE HOT LOADED:
HOTLOAD_VALUE = pp.Forward()
HOTLOAD_BIND = pp.Forward()

Time_VALBIND = pp.Or([PU.N(VALUE_S, HOTLOAD_VALUE),
                      PU.N(BIND_S, HOTLOAD_BIND)]) + PU.op(QUESTION)

pattern_contents = pp.OneOrMore(pp.Or([Time_VALBIND, acts]))

pattern = pp.Forward()
openers = pp.Or([PU.OPAR, OBRACKET, LESS])
closers = pp.Or([PU.CPAR, CBRACKET, MORE])

pattern <<= pp.Group(openers + pp.ZeroOrMore(pattern_contents | pattern)
                     + closers + PU.op(QUESTION))

main_pattern = PU.s(OBRACKET) + pattern + PU.s(CBRACKET)

# Actions
Time_VALBIND.setParseAction(make_valbind)
pattern.setParseAction(construct_pattern_simple)
main_pattern.setParseAction(lambda x: (PATTERN_S, x[0][0]))
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
