""" A Trie based Parser module for the creation of action """
import logging as root_logger
import pyparsing as pp
from acab.abstract import action as action
from acab.abstract.parsing import util as PU
from acab.working_memory.trie_wm import util as WMU

from .FactParser import PARAM_SEN, VALBIND, BASIC_SEN, PARAM_SEN

logging = root_logger.getLogger(__name__)


HOTLOAD_OPERATORS = pp.Forward()


def build_component(toks):
    action_vals = []
    if WMU.ACTION_VAL_S in toks:
        action_vals = toks[WMU.ACTION_VAL_S][:]

    return action.ActionComponent(toks[WMU.OPERATOR_S],
                                  action_vals)

def build_action(toks):
    # TODO remove hardcoded default
    clauses = [x if isinstance(x, action.ActionComponent)
               else action.ActionComponent('operator.action.add', [x]) for x in toks]
    act = action.Action(clauses)

    return (act.type, act)


# fact string with the option of binds
vals = pp.delimitedList(pp.Or([VALBIND, PARAM_SEN]), delim=PU.COMMA)

# action: [op](values)
action_component = PU.N(WMU.OPERATOR_S, HOTLOAD_OPERATORS) \
    + PU.OPAR + PU.op(PU.N(WMU.ACTION_VAL_S, vals)) + PU.CPAR

# Sentences are asserted by default
# TODO: block param_sen from beginning with "end"
actions = pp.delimitedList(pp.Or([action_component, PARAM_SEN]), delim=PU.DELIM)

action_definition = PU.STATEMENT_CONSTRUCTOR(PU.ACTION_HEAD,
                                             BASIC_SEN,
                                             actions + PU.component_gap)

# parse action
action_component.setParseAction(build_component)
actions.setParseAction(build_action)

# NAMING
vals.setName("ActionValueList")
action_component.setName("ActionComponent")
actions.setName("ActionsContainer")
action_definition.setName("ActionDefinition")

# parse_point = actions.ignore(PU.COMMENT)
parse_point = actions

def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
