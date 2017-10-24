""" Trie-based parser for the transform component of rules """
import logging as root_logger
import pyparsing as pp
import IPython
import pyRule.utils as util
from pyRule.Transforms import TROP, SelectionTransform, OperatorTransform, Transform
from .FactParser import COMMA, BIND, VALBIND
from .QueryParser import OPAR, CPAR, REGEX

logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
#names:
REBIND_N = 'rebind'
CORE_N = 'core'
#utils
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
ARROW = s(pp.Literal('->'))


#Builders:
def buildBinaryTransformComponent(toks):
    if isinstance(toks[2], util.Bind):
        val = None
        bind = toks[2]
    else:
        val = toks[2]
        bind = None
    return OperatorTransform(toks[1], toks[0], val, bind)

def buildUnaryTransformComponent(toks):
    operator = toks[0]
    source = toks[1]
    return OperatorTransform(operator, source)

def buildTernaryTransformComponent(toks):
    source = toks[0]
    operator = toks[1]
    regex = toks[2]
    bind = toks[3]
    return OperatorTransform(operator, source, value=regex, bind=bind)

def buildSelection(toks):
    bound1 = toks[0]
    bound2 = toks[1]
    return SelectionTransform(bound1, bound2, op=TROP.SELECT)


def addRebind(toks):
    component = toks[0]
    assert(component is not None)
    if REBIND_N in toks:
        reData = toks[REBIND_N][0]
        component.setRebind(reData)
    return component


#Transform Operators, TROPSs
#Binary:
ADD = pp.Literal('+').setParseAction(lambda t: TROP.ADD)
SUB = pp.Literal('-').setParseAction(lambda t: TROP.SUB)
MUL = pp.Literal('*').setParseAction(lambda t: TROP.MUL)
DIV = pp.Literal('/').setParseAction(lambda t: TROP.DIV)
REM = pp.Literal('%').setParseAction(lambda t: TROP.REMAIN)
RAND = pp.Literal('<->').setParseAction(lambda t: TROP.RAND)
REGEXSUB = pp.Literal('~=').setParseAction(lambda t: TROP.REGEX)
FORMAT = pp.Literal('~{}').setParseAction(lambda t: TROP.FORMAT)

#Unary:
ROUND = pp.Literal('_').setParseAction(lambda t: TROP.ROUND)
NEG = pp.Literal('-').setParseAction(lambda t: TROP.NEG)

binary_trops = pp.Or([ADD, SUB, MUL, DIV, REM, RAND])
unary_trops = pp.Or([ROUND, NEG, FORMAT])
ternary_trops = pp.Or([REGEXSUB])

rebind = (ARROW + BIND).setResultsName(REBIND_N)
selAll = pp.Literal('_').setParseAction(lambda toks: TROP.SELECT_ALL)

select = s(pp.Literal('select')) + pp.Or([selAll, VALBIND]) \
         + s(SUB) + pp.Or([selAll, VALBIND])


#transform: ( bind op val|bind -> bind)
unary_transform_core = unary_trops + pp.Or([BIND, pp.dblQuotedString])

binary_transform_core = VALBIND + binary_trops + VALBIND

ternary_transform_core = BIND + ternary_trops + REGEX + VALBIND

transform_core = pp.Or([unary_transform_core,
                        binary_transform_core,
                        ternary_transform_core]) \
                        + op(rebind)

transforms = pp.Or([select, transform_core]) \
             + pp.ZeroOrMore(COMMA + transform_core)

#Actions
binary_transform_core.setParseAction(buildBinaryTransformComponent)
unary_transform_core.setParseAction(buildUnaryTransformComponent)
ternary_transform_core.setParseAction(buildTernaryTransformComponent)
select.setParseAction(buildSelection)


transform_core.setParseAction(addRebind)
transforms.setParseAction(lambda toks: Transform(toks[:]))

def parseString(in_string):
    return transforms.parseString(in_string)[0]
