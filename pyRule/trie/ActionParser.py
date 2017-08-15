import logging as root_logger
import pyparsing as pp
from .FactParser import COMMA, param_fact_string
from .QueryParser import VALBIND, NOT, OPAR, CPAR
from .TransformParser import ADD
from pyRule import utils as util
from pyRule import Actions

logging = root_logger.getLogger(__name__)
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

#constructors:
def build_action(toks):
    return Actions.Action(toks.operator, toks.ActionValues[:])

#Action operators:
ASSERT = ADD.copy().setParseAction(lambda t: Actions.ACTS.ADD)
RETRACT = NOT.copy().setParseAction(lambda t: Actions.ACTS.RETRACT)
PRINT = pp.Literal('@').setParseAction(lambda t: Actions.ACTS.PRINT)
CUSTOM = pp.Word(pp.alphas)

#operators, also with a custom string option
operator = pp.Or([ASSERT, RETRACT, PRINT, CUSTOM])

#fact string with the option of binds
vals = pp.Or([param_fact_string, VALBIND]) \
       + pp.ZeroOrMore(COMMA + pp.Or([param_fact_string, VALBIND]))

# action: [op](values)
action = operator.setResultsName('operator') + OPAR \
         + vals.setResultsName('ActionValues') \
         + CPAR 

actions = OPAR + action + pp.ZeroOrMore(COMMA + action) + CPAR

#parse actions
action.setParseAction(build_action)

def parseString(s):
    return action.parseString(s)

def parseStrings(s):
    return actions.parseString(s)
