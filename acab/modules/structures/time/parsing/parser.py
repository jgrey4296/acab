""""
A PyParsing parser to create patterns
"""
from fractions import Fraction
import pyparsing as pp
import logging as logmod

from acab.core.parsing import consts as PC
from acab.core.parsing.consts import QUERY_SYMBOL, N, NG, OPAR, CPAR
from acab.modules.structures.time.pattern_constructor import CTOR_ACT
from acab.modules.structures.time.pattern_constructor import construct_pattern_simple
from acab.modules.structures.time.util import BIND_S, TYPE_INSTANCE_S, VALUE_S, NAME_S, OPT_S, PATTERN_S

from acab.core.parsing import parsers as PU

logging = logmod.getLogger(__name__)


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

acts = COMMA | PC.COLON | TILDE

# TO BE HOT LOADED:
HOTLOAD_VALUE = pp.Forward()
HOTLOAD_VALUE.set_name('hotload_value')
HOTLOAD_BIND = pp.Forward()
HOTLOAD_BIND.set_name('hotload_bind')

Time_VALBIND = (HOTLOAD_VALUE(VALUE_S) | HOTLOAD_BIND(BIND_S)) + PU.op(QUESTION)

pattern_contents = pp.OneOrMore(Time_VALBIND | acts)

pattern = pp.Forward()
openers = OPAR |  OBRACKET |  LESS
closers = CPAR |  CBRACKET |  MORE

pattern <<= pp.Group(openers + pp.ZeroOrMore(pattern_contents | pattern)
                     + closers + PU.op(QUESTION))

main_pattern = PU.s(OBRACKET) + pattern + PU.s(CBRACKET)

# Actions
Time_VALBIND.set_parse_action(make_valbind)
pattern.set_parse_action(construct_pattern_simple)
main_pattern.set_parse_action(lambda s, l, t: (PATTERN_S, t[0][0]))
OBRACKET.set_parse_action(lambda x: CTOR_ACT.PSTART)
CBRACKET.set_parse_action(lambda x: CTOR_ACT.PEND)
COMMA.set_parse_action(lambda x: CTOR_ACT.PDUAL)
QUESTION.set_parse_action(lambda x: CTOR_ACT.OP)
LESS.set_parse_action(lambda x: CTOR_ACT.CSTART)
MORE.set_parse_action(lambda x: CTOR_ACT.CEND)
TILDE.set_parse_action(lambda x: (CTOR_ACT.SIL, {}))

# NAMING
acts.set_name("TimePatternActs")
pattern_contents.set_name("TimePatternContents")
pattern.set_name("TimePattern")
openers.set_name("TimeOpeners")
closers.set_name("TimeClosers")

# parse_point = main_pattern.ignore(PU.COMMENT)
parse_point = main_pattern

def parse_string(s):
    """ The primary access point """
    return parse_point.parse_string(s)[0][1]
