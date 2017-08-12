import logging as root_logger
import pyparsing as pp
from .FactParser import COMMA, BIND, VALBIND
from .QueryParser import OPAR, CPAR
import pyRule.utils as util

logging = root_logger.getLogger(__name__)
#names:
REBIND_N = 'rebind'

#utils
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
ARROW = s(pp.Literal('->'))


#Builders:
def buildTransformComponent(toks):
    if isinstance(toks[2], util.Bind):
        val = None
        bind = toks[2]
    else:
        val = toks[2]
        bind = None

    if REBIND_N in toks:
        reData = toks[REBIND_N][0]
    else:
        reData = None
    return util.TransformComponent(toks[1], toks[0], val, bind, reData)


#TROPS:
ADD = pp.Literal('+').setParseAction(lambda t: util.TROP.ADD)
SUB = pp.Literal('-').setParseAction(lambda t: util.TROP.SUB)
MUL = pp.Literal('*').setParseAction(lambda t: util.TROP.MUL)
DIV = pp.Literal('/').setParseAction(lambda t: util.TROP.DIV)

TROP = pp.Or([ADD, SUB, MUL, DIV])

rebind = (ARROW + BIND).setResultsName(REBIND_N)

#transform: ( bind op val|bind -> bind)
#todo: separate out single operator trops and multi operator trops
transform_core = BIND + TROP + VALBIND + op(rebind)
transform = OPAR + transform_core + CPAR

transforms = OPAR + transform_core + pp.ZeroOrMore(COMMA + transform_core) + CPAR

#Actions
transform_core.setParseAction(buildTransformComponent)


def parseString(s):
    return transform.parseString(s)
    
def parseStrings(s):
    return transforms.parseString(s)
