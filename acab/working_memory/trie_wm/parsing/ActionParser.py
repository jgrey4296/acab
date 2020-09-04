""" A Trie based Parser module for the creation of action """
import logging as root_logger
import pyparsing as pp

from acab.abstract.core.sentence import Sentence
from acab.abstract.rule import action as action
from acab.abstract.parsing import util as PU

from acab.config import AcabConfig

from .FactParser import PARAM_SEN, VALBIND, BASIC_SEN, PARAM_SEN

logging = root_logger.getLogger(__name__)
util = AcabConfig.Get()

DEFAULT_ACTION_S = util("Parsing.Structure", "DEFAULT_ACTION_S")

LEFT_S = util("WorkingMemory.TrieWM", "LEFT_S")
RIGHT_S = util("WorkingMemory.TrieWM", "RIGHT_S")
OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")


HOTLOAD_OPERATORS = pp.Forward()


def build_action_component(toks):
    params = []
    if LEFT_S in toks:
        params.append(toks[LEFT_S])
    if RIGHT_S in toks:
        params = toks[RIGHT_S][:]
    op = toks[OPERATOR_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]
    return action.ActionComponent(op, filtered_params, sugared=LEFT_S in toks)

def build_action(toks):
    clauses = [x if isinstance(x, action.ActionComponent)
               else action.ActionComponent(Sentence.build([DEFAULT_ACTION_S]), [x]) for x in toks]
    act = action.Action(clauses)

    return (act.type, act)


# fact string with the option of binds
vals = PU.N(RIGHT_S, PU.zrm(PARAM_SEN))

op_path = PU.OP_PATH_C(BASIC_SEN)

# action: [op](values)
action_component = PU.N(OPERATOR_S, op_path) + vals

action_sugar = PU.N(LEFT_S, VALBIND) \
    + PU.N(OPERATOR_S, HOTLOAD_OPERATORS) \
    + vals

# Sentences are asserted by default
actions = pp.delimitedList(pp.Or([action_component, PARAM_SEN]), delim=PU.DELIM)

action_definition = PU.STATEMENT_CONSTRUCTOR(PU.ACTION_HEAD,
                                             BASIC_SEN,
                                             actions + PU.component_gap)

# parse action
action_component.setParseAction(build_action_component)
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
