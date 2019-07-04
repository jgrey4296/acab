""""
A PyParsing parser to create patterns
"""
# from .pattern import Pattern
# from .event import Event
# from .arc import Arc
from py_rule.datastructures.time.pattern_constructor import CTOR_ACT, construct_pattern
from py_rule.datastructures.time.utils import TimeVar
from enum import Enum
from fractions import Fraction
import pyparsing as pp
import logging as root_logger

logging = root_logger.getLogger(__name__)
#Base Syntax
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.lineEnd))
comment = s(pp.dblSlashComment)

COMMA = s(pp.Literal(','))
OBRACKET = s(pp.Literal('['))
CBRACKET = s(pp.Literal(']'))
COLON = s(pp.Literal(':'))
QUESTION = s(pp.Literal('?'))
LESS = s(pp.Literal('<'))
MORE = s(pp.Literal('>'))
TILDE = s(pp.Literal('~'))
DOLLAR = s(pp.Literal('$'))
VBAR = s(pp.Literal('|'))

acts = pp.Or([COMMA, OBRACKET, CBRACKET, COLON, QUESTION, LESS, MORE, TILDE])

NAME = pp.Word(pp.alphas)
VAR = DOLLAR + NAME

pattern_contents = pp.OneOrMore(pp.Or([VAR, NAME, acts]))

# Actions
OBRACKET.setParseAction(lambda x: CTOR_ACT.PSTART)
CBRACKET.setParseAction(lambda x: CTOR_ACT.PEND)
COMMA.setParseAction(lambda x: CTOR_ACT.PDUAL)
QUESTION.setParseAction(lambda x: CTOR_ACT.OP)
LESS.setParseAction(lambda x: CTOR_ACT.CSTART)
MORE.setParseAction(lambda x: CTOR_ACT.CEND)
TILDE.setParseAction(lambda x: CTOR_ACT.SIL)

VAR.setParseAction(lambda toks: TimeVar(toks[0]))

def parse_string(s):
    """ The primary access point """
    parsed = pattern_contents.parseString(s)
    # Stack based constructor:
    return construct_pattern(parsed[:])

