"""
Pyparsing based parser to turn strings into [FactNode],
both in the single fact form (parseString), and multifact form (parseStrings)
"""
import pyparsing as pp
import logging as root_logger
from .FactNode import FactNode
from .utils import EXOP
import IPython

#UTILITIES
logging = root_logger.getLogger(__name__)
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

def construct_num(tok):
    #todo: add in fractions and underscores
    if 'd' in toks:
        return float(tok.replace('d','.'))
    else:
        return int(tok)

#Base Defs
DOT = pp.Keyword('.', identChars='!')
EX = pp.Keyword('!', identChars='.')
OP = pp.Or([DOT,EX])
COMMA = pp.Literal(',')

NAME = pp.Word(pp.alphas)
NUM = pp.Word(pp.nums + '-d')
STRING = pp.dblQuotedString
VALUE = pp.Or([NAME, NUM, STRING])

#Core = .a | !b
CORE = OP + VALUE

#fact string
fact_string = pp.OneOrMore(CORE)
fact_strings = fact_string + pp.ZeroOrMore(s(COMMA) + fact_string)

#Actions
DOT.setParseAction(lambda t: EXOP.DOT)
EX.setParseAction(lambda t: EXOP.EX)
NUM.setParseAction(lambda t: int(t[0]))
CORE.setParseAction(lambda toks: FactNode(toks[1], toks[0]))
fact_string.setParseAction(lambda toks: [toks[:]])

# MAIN PARSER:
def parseString(s):
    """ str -> [FactNode] """
    parsed = fact_string.parseString(s)[0]
    return [FactNode.Root()] + parsed

def parseStrings(s):
    """ str -> [[FactNode]] """
    parsed = fact_strings.parseString(s)[:]
    with_roots = [[FactNode.Root()] + x for x in parsed]
    return with_roots

