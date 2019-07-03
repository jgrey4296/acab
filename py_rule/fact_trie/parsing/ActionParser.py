""" A Trie based Parser module for the creation of Actions """
import logging as root_logger
import pyparsing as pp
from py_rule import utils as util
from py_rule.abstract import actions as Actions
import IPython

from .FactParser import COMMA, param_fact_string, end, COLON, sLn, comment, N, PARAM_CORE
from .QueryParser import NOT, OPAR, CPAR, BIND

ACTION_STRS = [x for x in Actions.ActionOp.op_list.keys()]

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
    if 'binds' in toks:
        parameters = toks.binds
    if 'actions' in toks:
        m_actions = toks.actions

    return Actions.ActionMacro(toks.name,
                               parameters,
                               m_actions)


#Action operators:
CUSTOM = pp.Word(pp.alphas)
operator = pp.Or([pp.Literal(x) for x in ACTION_STRS] + [CUSTOM])

ACT_MACRO = (s(pp.Literal('#')) + CUSTOM).setParseAction(lambda t: Actions.ACTMACRONAME(t[0]))


#fact string with the option of binds
vals = param_fact_string + pp.ZeroOrMore(COMMA + param_fact_string)

bindList = BIND + pp.ZeroOrMore(COMMA + BIND)
# action: [op](values)
action = N('operator', operator) + OPAR + N("ActionValues", vals) + CPAR

actionMacroUse = ACT_MACRO + \
        OPAR + N("bindings", op(vals)) + CPAR

actionsOrMacros = pp.Or([actionMacroUse, action])

justActions = action + pp.ZeroOrMore(COMMA + action)
actions = actionsOrMacros + pp.ZeroOrMore(COMMA + actionsOrMacros)

action_definition = N("name", ACT_MACRO) + OPAR \
                    + N("binds", op(bindList)) \
                    + CPAR + COLON + sLn\
                    + N("actions", justActions) \
                    + sLn + end


#parse actions
action.setParseAction(build_action)
actionMacroUse.setParseAction(build_macro_use)
action_definition.setParseAction(build_definition)

def parseString(in_string):
    return actions.parseString(in_string)[:]
