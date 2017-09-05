""" A Trie based Parser module for the creation of Actions """
import logging as root_logger
import pyparsing as pp
from pyRule import utils as util
from pyRule import Actions
import IPython

from .FactParser import COMMA, param_fact_string, end, COLON, sLn, comment
from .QueryParser import VALBIND, NOT, OPAR, CPAR, BIND
from .TransformParser import ADD


logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

#constructors:
def build_action(toks):
    return Actions.Action(toks.operator, toks.ActionValues[:])

def build_macro_use(toks):
    if 'bindings' in toks:
        bindings = toks.bindings[:]
    else:
        bindings = []
    return Actions.ActionMacroUse(toks[0],
                                  bindings)

def build_definition(toks):
    parameters = []
    m_actions = []
    for x in toks[1:]:
        if isinstance(x, util.Bind):
            parameters.append(x)
        elif isinstance(x, Actions.Action):
            m_actions.append(x)

    return Actions.ActionMacro(toks[0],
                               parameters,
                               m_actions)

#Action operators:
ASSERT = pp.Literal('+').setParseAction(lambda t: Actions.ACTS.ADD)
RETRACT = pp.Literal('-').setParseAction(lambda t: Actions.ACTS.RETRACT)
PRINT = pp.Literal('@').setParseAction(lambda t: Actions.ACTS.PRINT)
CUSTOM = pp.Word(pp.alphas)
ACT_MACRO = (s(pp.Literal('#')) + CUSTOM).setParseAction(lambda t: Actions.ACTMACRONAME(t[0]))

#operators, also with a custom string option
operator = pp.Or([ASSERT, RETRACT, PRINT, CUSTOM])

#fact string with the option of binds
vals = pp.Or([param_fact_string, VALBIND]) \
       + pp.ZeroOrMore(COMMA + pp.Or([param_fact_string, VALBIND]))

bindList = BIND + pp.ZeroOrMore(COMMA + BIND)
# action: [op](values)
action = operator.setResultsName('operator') + OPAR \
         + vals.setResultsName('ActionValues') \
         + CPAR

actionMacroUse = ACT_MACRO + \
        OPAR + op(vals).setResultsName('bindings') + CPAR

actionsOrMacros = pp.Or([actionMacroUse, action])

justActions = action + pp.ZeroOrMore(COMMA + action)
actions = actionsOrMacros + pp.ZeroOrMore(COMMA + actionsOrMacros)

action_definition = ACT_MACRO + OPAR \
                    + op(bindList) \
                    + CPAR + COLON + sLn\
                    + justActions \
                    + sLn + end




#parse actions
action.setParseAction(build_action)
actionMacroUse.setParseAction(build_macro_use)
action_definition.setParseAction(build_definition)

def parseString(in_string):
    return actions.parseString(in_string)[:]
