""" A Trie based Parser module for the creation of action """
import logging as root_logger
import pyparsing as pp
from acab.abstract import action as action
from acab.abstract.parsing import util as PU
from acab.abstract.sentence import Sentence
from acab.working_memory.trie_wm import util as WMU

from .FactParser import PARAM_SEN, VALBIND, BASIC_SEN, PARAM_SEN

logging = root_logger.getLogger(__name__)


HOTLOAD_OPERATORS = pp.Forward()


def build_component(toks):
    action_vals = []
    if WMU.ACTION_VAL_S in toks:
        action_vals = toks[WMU.ACTION_VAL_S][:]
    op = toks[WMU.OPERATOR_S][0]
    return action.ActionComponent(op, action_vals)

def build_action(toks):
    # TODO remove hardcoded default
    clauses = [x if isinstance(x, action.ActionComponent)
               else action.ActionComponent(Sentence.build(['DEFAULT_ACTION']), [x]) for x in toks]
    act = action.Action(clauses)

    return (act.type, act)


# fact string with the option of binds
vals = pp.delimitedList(pp.Or([VALBIND, PARAM_SEN]), delim=PU.COMMA)

op_path = pp.Or([HOTLOAD_OPERATORS, PU.OP_PATH_C(BASIC_SEN)])

# action: [op](values)
action_component = PU.N(WMU.OPERATOR_S, op_path) \
    + PU.op(PU.OPAR + PU.N(WMU.ACTION_VAL_S, vals) + PU.CPAR)

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
HOTLOAD_OPERATORS.setName("HotloadOp")

# parse_point = actions.ignore(PU.COMMENT)
parse_point = actions

def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
