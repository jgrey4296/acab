import logging as root_logger
import pyparsing as pp
from .FactParser import COMMA, BIND, VALBIND
from .QueryParser import OPAR, CPAR, REGEX
from pyRule.Transforms import TROP, TransformComponent, Transform
import pyRule.utils as util
import IPython

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
    return TransformComponent(toks[1], toks[0], val, bind)

def buildUnaryTransformComponent(toks):
    op = toks[0]
    source = toks[1]
    return TransformComponent(op, source)

def buildTernaryTransformComponent(toks):
    source = toks[0]
    op = toks[1]
    regex = toks[2]
    bind = toks[3]
    return TransformComponent(op, source, value=regex, bind=bind)

    
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

#Unary:
ROUND = pp.Literal('_').setParseAction(lambda t: TROP.ROUND)
NEG = pp.Literal('-').setParseAction(lambda t: TROP.NEG)
SLASH = pp.Literal('/')

binary_trops = pp.Or([ADD, SUB, MUL, DIV, REM, RAND])
unary_trops = pp.Or([ROUND, NEG])
ternary_trops = pp.Or([REGEXSUB])

rebind = (ARROW + BIND).setResultsName(REBIND_N)

#todo: spec the bounds of contexts to select to transform and act upon
select = s(pp.Literal('select')) + VALBIND + s(SUB) + VALBIND


#transform: ( bind op val|bind -> bind)
#todo: separate out single operator trops and multi operator trops
unary_transform_core = unary_trops + BIND

binary_transform_core = VALBIND + binary_trops + VALBIND

ternary_transform_core = BIND + ternary_trops + REGEX + VALBIND

transform_core = pp.Or([unary_transform_core,
                        binary_transform_core,
                        ternary_transform_core]) \
                        + op(rebind)

transforms = op(select + COMMA) \
             + transform_core \
             + pp.ZeroOrMore(COMMA + transform_core)

#Actions
binary_transform_core.setParseAction(buildBinaryTransformComponent)
unary_transform_core.setParseAction(buildUnaryTransformComponent)
ternary_transform_core.setParseAction(buildTernaryTransformComponent)

transform_core.setParseAction(addRebind)
transforms.setParseAction(lambda toks: Transform(toks[:]))

#todo: build binary, unary, and ternary transforms,
#build selection operator, build format

def parseString(s):
    return transforms.parseString(s)[0]
