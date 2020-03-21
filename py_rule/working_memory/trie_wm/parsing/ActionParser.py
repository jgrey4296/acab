""" A Trie based Parser module for the creation of Actions """
import logging as root_logger
import pyparsing as pp
from py_rule.abstract import action as Actions
from py_rule.abstract.parsing import util as PU
from py_rule.working_memory.trie_wm import util as WMU

from .FactParser import PARAM_SEN, VALBIND, BASIC_SEN

logging = root_logger.getLogger(__name__)


HOTLOAD_OPERATORS = pp.Forward()

def build_operators():
    """ For Hotloading Action operators """
    if HOTLOAD_OPERATORS.expr is not None:
        logging.warning("Action Operators Overwrite")
    ACTION_STRS = [x for x in Actions.ActionOp.op_list.keys()]
    HOTLOAD_OPERATORS << pp.Or([pp.Literal(x) for x in ACTION_STRS] + [PU.OPERATOR_SUGAR])


def build_component(toks):
    return Actions.ActionComponent(toks[WMU.OPERATOR_S],
                                   toks[WMU.ACTION_VAL_S][:])

def build_action(toks):
    return Actions.Action(toks[:])

def build_macro_use(toks):
    if WMU.BIND_S in toks:
        bindings = toks[WMU.BIND_S][:]
    else:
        bindings = []
    return Actions.ActionMacroUse(toks[0],
                                  bindings)


def build_definition(toks):
    parameters = []
    m_actions = []
    if WMU.BIND_S in toks:
        parameters = toks[WMU.BIND_S][:]
    if WMU.ACTION_S in toks:
        m_actions = toks[WMU.ACTION_S][:]

    sen = toks[WMU.NAME_S][0]
    sen[-1]._data[WMU.VALUE_S] = Actions.Action(m_actions, params=parameters)
    sen[-1]._data[WMU.VALUE_TYPE_S] = WMU.ACTION_S

    return sen


ACT_MACRO = PU.MACRO_HEAD + PU.DBLCOLON + BASIC_SEN

# fact string with the option of binds
vals = pp.delimitedList(PARAM_SEN, delim=PU.COMMA)

bindList = pp.delimitedList(VALBIND, delim=PU.COMMA)
# action: [op](values)
action_component = PU.N(WMU.OPERATOR_S, HOTLOAD_OPERATORS) \
    + PU.OPAR + PU.N(WMU.ACTION_VAL_S, vals) + PU.CPAR

actionMacroUse = ACT_MACRO + \
        PU.OPAR + PU.N(WMU.BIND_S, PU.op(vals)) + PU.CPAR

actionsOrMacros = pp.Or([actionMacroUse, action_component])

justActions = pp.delimitedList(action_component, delim=PU.COMMA)
actions = pp.delimitedList(actionsOrMacros, delim=PU.COMMA)

action_definition = PU.NG(WMU.NAME_S, ACT_MACRO) + PU.OPAR \
                    + PU.NG(WMU.BIND_S, PU.op(bindList)) \
                    + PU.CPAR + PU.COLON + PU.sLn\
                    + PU.NG(WMU.ACTION_S, justActions) \
                    + PU.sLn + PU.end


# parse actions
action_component.setParseAction(build_component)
actions.setParseAction(build_action)
actionMacroUse.setParseAction(build_macro_use)
action_definition.setParseAction(build_definition)
ACT_MACRO.setParseAction(lambda t: t[0])


def parseString(in_string):
    return actions.parseString(in_string)[0]
