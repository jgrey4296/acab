""" A Trie based Parser module for the creation of action """
import logging as root_logger
import pyparsing as pp
from py_rule.abstract import action as action
from py_rule.abstract.parsing import util as PU
from py_rule.working_memory.trie_wm import util as WMU

from .FactParser import PARAM_SEN, VALBIND, BASIC_SEN

logging = root_logger.getLogger(__name__)


HOTLOAD_OPERATORS = pp.Forward()

def build_operators():
    """ For Hotloading Action operators """
    if HOTLOAD_OPERATORS.expr is not None:
        logging.warning("Action Operators Overwrite")
    ACTION_STRS = [x for x in action.ActionOp.op_list.keys()]
    HOTLOAD_OPERATORS << pp.Or([pp.Literal(x) for x in ACTION_STRS] + [PU.OPERATOR_SUGAR])


def build_component(toks):
    return action.ActionComponent(toks[WMU.OPERATOR_S],
                                   toks[WMU.ACTION_VAL_S][:])


def build_action(toks):
    act = action.Action(toks[:])
    return (act._type, act)

# fact string with the option of binds
vals = pp.delimitedList(PARAM_SEN, delim=PU.COMMA)

bindList = pp.delimitedList(VALBIND, delim=PU.COMMA)
# action: [op](values)
action_component = PU.N(WMU.OPERATOR_S, HOTLOAD_OPERATORS) \
    + PU.OPAR + PU.N(WMU.ACTION_VAL_S, vals) + PU.CPAR

actions = pp.delimitedList(action_component, delim=PU.DELIM)

action_definition = PU.STATEMENT_CONSTRUCTOR(PU.ACTION_HEAD, BASIC_SEN, actions)

# parse action
action_component.setParseAction(build_component)
actions.setParseAction(build_action)


def parseString(in_string):
    return actions.parseString(in_string)[0][1]
