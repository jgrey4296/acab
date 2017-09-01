import logging as root_logger
import pyparsing as pp
from .FactParser import COMMA, param_fact_string, end, COLON
from .QueryParser import VALBIND, NOT, OPAR, CPAR, BIND
from .TransformParser import ADD
from pyRule import utils as util
from pyRule import Actions

logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

#constructors:
def build_action(toks):
    return Actions.Action(toks.operator, toks.ActionValues[:])

def build_macro_use(toks):
    return Actions.ActionMacroUse(toks.name,
                                  toks.bindings)

def build_definition(toks):
    return Actions.ActionMacro(toks.actDefName,
                               toks.actDefParams,
                               toks.actDefActions)

#Action operators:
ASSERT = pp.Literal('+').setParseAction(lambda t: Actions.ACTS.ADD)
RETRACT = pp.Literal('-').setParseAction(lambda t: Actions.ACTS.RETRACT)
PRINT = pp.Literal('@').setParseAction(lambda t: Actions.ACTS.PRINT)
CUSTOM = pp.Word(pp.alphas)
ACT_MACRO = (s(pp.Literal('#')) + CUSTOM).setParseAction(lambda t: Actions.ACTMACRONAME(toks[0]))

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

actionMacroUse = ACT_MACRO.setResultsName('macroName') + \
        OPAR + op(bindList).setResultsName('bindings') + CPAR

actionsOrMacros = pp.Or([actionMacroUse, action])

actions = actionsOrMacros + pp.ZeroOrMore(COMMA + actionsOrMacros)

action_definition = ACT_MACRO.setResultsName("actDefName") \
                    + OPAR \
                    + op(bindList).setResultsName("actDefParams") \
                    + CPAR + COLON \
                    + actions.setResultsName("actDefActions") \
                    + end


#parse actions
action.setParseAction(build_action)
actionMacroUse.setParseAction(build_macro_use)
action_definition.setParseAction(build_definition)

def parseString(s):
    return actions.parseString(s)[:]
