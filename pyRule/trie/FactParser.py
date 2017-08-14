"""
Pyparsing based parser to turn strings into [FactNode],
both in the single fact form (parseString), and multifact form (parseStrings)
"""
import pyparsing as pp
import logging as root_logger
from .Node import Node
from pyRule.utils import EXOP, Bind
import IPython

#UTILITIES
logging = root_logger.getLogger(__name__)
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

def construct_num(toks):
    #todo: add in fractions and underscores
    if 'd' in toks:
        return float(toks.replace('d','.'))
    else:
        return int(toks)

#Base Defs
DOT = pp.Keyword('.', identChars='!')
EX = pp.Keyword('!', identChars='.')
OP = pp.Or([DOT,EX])
COMMA = s(pp.Literal(',') + opLn)
DOLLAR = pp.Literal('$')

NAME = pp.Word(pp.alphas)
NUM = pp.Word(pp.nums + '-d').setParseAction(construct_num)
STRING = pp.dblQuotedString
VALUE = pp.Or([NAME, NUM, STRING])

#alt for actions, PARAM_CORE
BIND = DOLLAR + VALUE
VALBIND = pp.Or([VALUE, BIND])

#Core = .a | !b | .$a | !$b
PARAM_CORE = OP + VALBIND

param_fact_string = pp.OneOrMore(PARAM_CORE)
param_fact_strings = param_fact_string + pp.ZeroOrMore(COMMA + param_fact_string)

#Actions
BIND.setParseAction(lambda toks: Bind(toks[1]))
STRING.setParseAction(lambda t: t[0].replace('"',''))
DOT.setParseAction(lambda t: EXOP.DOT)
EX.setParseAction(lambda t: EXOP.EX)
NUM.setParseAction(lambda t: construct_num(t[0]))

PARAM_CORE.setParseAction(lambda toks: Node(toks[1], toks[0]))
param_fact_string.setParseAction(lambda toks: [toks[:]])

# MAIN PARSER:
def parseString(s):
    """ str -> [Node] """
    parsed = param_fact_string.parseString(s)[0]
    return [Node.Root()] + parsed

def parseStrings(s):
    """ str -> [[Node]] """
    parsed = param_fact_strings.parseString(s)[:]
    with_roots = [[Node.Root()] + x for x in parsed]
    return with_roots

