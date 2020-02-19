""" A Trie based Parser module for the creation of Actions """
import logging as root_logger
import pyparsing as pp
from py_rule.abstract import action as Actions
from py_rule.abstract.parsing import util as PU
from py_rule.knowledge_bases.trie_kb import util as kb_util

from .FactParser import param_fact_string, VALBIND

logging = root_logger.getLogger(__name__)


def build_operators():
    """ For Hotloading Action operators """
    if operator.expr is None:
        ACTION_STRS = [x for x in Actions.ActionOp.op_list.keys()]
        operator << pp.Or([pp.Literal(x) for x in ACTION_STRS] + [CUSTOM])


# constructors:
def build_action(toks):
    return Actions.Action(toks[kb_util.OPERATOR_S],
                          toks[kb_util.ACTION_VAL_S][:])


def build_macro_use(toks):
    if kb_util.BIND_S in toks:
        bindings = toks[kb_util.BIND_S][:]
    else:
        bindings = []
    return Actions.ActionMacroUse(toks[0],
                                  bindings)


def build_definition(toks):
    parameters = []
    m_actions = []
    if kb_util.BIND_S in toks:
        parameters = toks[kb_util.BIND_S][:]
    if kb_util.ACTION_S in toks:
        m_actions = toks[kb_util.ACTION_S][:]

    return Actions.ActionMacro(toks[kb_util.NAME_S][0],
                               parameters,
                               m_actions)


# Action operators:
CUSTOM = pp.Word(pp.alphas)

operator = pp.Forward()

ACT_MACRO = PU.s(PU.HASH) + CUSTOM

# fact string with the option of binds
vals = param_fact_string + pp.ZeroOrMore(PU.COMMA + param_fact_string)

bindList = VALBIND + pp.ZeroOrMore(PU.COMMA + VALBIND)
# action: [op](values)
action = PU.N(kb_util.OPERATOR_S, operator) \
    + PU.OPAR + PU.N(kb_util.ACTION_VALUE_S, vals) + PU.CPAR

actionMacroUse = ACT_MACRO + \
        PU.OPAR + PU.N(kb_util.BIND_S, PU.op(vals)) + PU.CPAR

actionsOrMacros = pp.Or([actionMacroUse, action])

justActions = action + pp.ZeroOrMore(PU.COMMA + action)
actions = actionsOrMacros + pp.ZeroOrMore(PU.COMMA + actionsOrMacros)

action_definition = PU.NG(kb_util.NAME_S, ACT_MACRO) + PU.OPAR \
                    + PU.NG(kb_util.BIND_S, PU.op(bindList)) \
                    + PU.CPAR + PU.COLON + PU.sLn\
                    + PU.NG(kb_util.ACTION_S, justActions) \
                    + PU.sLn + PU.end


# parse actions
action.setParseAction(build_action)
actionMacroUse.setParseAction(build_macro_use)
action_definition.setParseAction(build_definition)
ACT_MACRO.setParseAction(lambda t: t[0])


def parseString(in_string):
    return actions.parseString(in_string)[:]
