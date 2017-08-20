"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import pyparsing as pp
import logging as root_logger
from .Node import Node
from pyRule.utils import EXOP, Bind
import IPython

#UTILITIES
logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
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
COMMA = s(pp.Literal(',') + opLn + op(pp.White('\t')))
DOLLAR = pp.Literal('$')

#todo: add in underscores
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
param_fact_string.setParseAction(lambda toks: [[Node.Root()] + toks[:]])

# MAIN PARSER:
def parseStrings(s):
    """ str -> [[Node]] """
    parsed = param_fact_strings.parseString(s)[:]
    return parsed

