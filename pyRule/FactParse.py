import pyparsing as pp
import logging as root_logger
from .FactNode import FactNode, EXOP

#UTILITIES
logging = root_logger.getLogger(__name__)
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

def construct_num(tok):
    if 'd' in toks:
        return float(tok.replace('d','.'))
    else:
        return int(tok)

#Base Defs
DOT = pp.Keyword('.', identChars='!')
EX = pp.Keyword('!', identChars='.')
OP = pp.Or([DOT,EX])

NAME = pp.Word(pp.alphas)
NUM = pp.Word(pp.nums + '-d')
STRING = pp.dblQuotedString
VALUE = pp.Or([NAME, NUM, STRING])

#Core = .a | !b
CORE = OP + VALUE

#fact string
fact_string = pp.OneOrMore(CORE)

#Actions
DOT.setParseAction(lambda t: EXOP.DOT)
EX.setParseAction(lambda t: EXOP.EX)
NUM.setParseAction(lambda t: int(t[0]))
CORE.setParseAction(lambda toks: FactNode(toks[1], toks[0]))

# MAIN PARSER:
def parseString(s):
    """ str -> [FactNode] """
    factList = [FactNode.Root()]
    return factList + fact_string.parseString(s)[:]

