""""
A PyParsing parser to create patterns
"""
# from .pattern import Pattern
# from .event import Event
# from .arc import Arc
from py_rule.modules.time.pattern_constructor import CTOR_ACT, construct_pattern_simple
from py_rule.knowledge_bases.trie_kb.parsing import FactParser as FP
from py_rule.utils import BIND_S, VALUE_TYPE_S, VALUE_S, NAME_S, OPT_S
from enum import Enum
from fractions import Fraction
import pyparsing as pp
import logging as root_logger

logging = root_logger.getLogger(__name__)

def make_valbind(tokens):
    data = {BIND_S : False,
            OPT_S : False,
            VALUE_TYPE_S : NAME_S }
    value = None
    if BIND_S in tokens:
        value = tokens.bind[0][1]
        data[BIND_S] = True
    elif VALUE_S in tokens:
        value = tokens.value[1]
        data[VALUE_TYPE_S] = tokens.value[0]

    if OPT_S in tokens:
        data[OPT_S] = True

    return (value, data)

#Base Syntax
s = FP.s
op = FP.op
opLn = FP.opLn

COMMA = s(pp.Literal(','))
OBRACKET = s(pp.Literal('['))
CBRACKET = s(pp.Literal(']'))
OPAR = s(pp.Literal('('))
CPAR = s(pp.Literal(')'))
COLON = s(pp.Literal(':'))
QUESTION = s(pp.Literal('?')).setResultsName(OPT_S)
LESS = s(pp.Literal('<'))
MORE = s(pp.Literal('>'))
TILDE = s(pp.Literal('~'))
DOLLAR = s(pp.Literal('$'))
VBAR = s(pp.Literal('|'))
ARROW = s(pp.Literal('->'))

acts = pp.Or([COMMA, COLON, TILDE])

VALUE = FP.VALUE
BIND = FP.BIND

Time_VALBIND = pp.Or([FP.N(VALUE_S, VALUE), FP.N(BIND_S, BIND)]) + op(QUESTION)

pattern_contents = pp.OneOrMore(pp.Or([Time_VALBIND, acts]))

pattern = pp.Forward()
openers = pp.Or([OPAR, OBRACKET, LESS])
closers = pp.Or([CPAR, CBRACKET, MORE])

pattern <<= pp.Group( openers + pp.ZeroOrMore( pattern_contents | pattern )  + closers + op(QUESTION))

main_pattern = s(OBRACKET) + pattern + s(CBRACKET)

# Actions
Time_VALBIND.setParseAction(make_valbind)
pattern.setParseAction(construct_pattern_simple)
main_pattern.setParseAction(lambda x: ("pattern", x[0][0]))
OBRACKET.setParseAction(lambda x: CTOR_ACT.PSTART)
CBRACKET.setParseAction(lambda x: CTOR_ACT.PEND)
COMMA.setParseAction(lambda x: CTOR_ACT.PDUAL)
QUESTION.setParseAction(lambda x: CTOR_ACT.OP)
LESS.setParseAction(lambda x: CTOR_ACT.CSTART)
MORE.setParseAction(lambda x: CTOR_ACT.CEND)
TILDE.setParseAction(lambda x: (CTOR_ACT.SIL, {}))

def parse_string(s):
    """ The primary access point """
    return main_pattern.parseString(s)[0][1]
