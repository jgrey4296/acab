""" A Trie based Parser module for the creation of action """
import logging as root_logger

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import DELIM, NG, N, component_gap, orm, zrm
from acab.core.parsing.default_keys import OPERATOR
from acab.core.parsing.default_symbols import ACTION_HEAD
from acab.core.parsing.parsers import VALBIND
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.util import LEFT_S, RIGHT_S, ACTION_HEAD
from acab.core.parsing.indented_block import IndentedBlock

from .FactParser import SENTENCE, op_sentence

logging = root_logger.getLogger(__name__)

HOTLOAD_OPERATORS         = pp.Forward()
HOTLOAD_OPERATORS.set_name("hotload_operators")
HOTLOAD_ACTION_STATEMENTS = pp.Forward()
HOTLOAD_ACTION_STATEMENTS.set_name("hotload_action_statements")

# fact string with the option of binds
vals = PU.zrm(SENTENCE)(RIGHT_S)

# action: [op](values)
action_component    = N(OPERATOR, op_sentence) + vals
action_component.set_parse_action(PConst.build_action_component)

action_sugar_binary = N(LEFT_S, VALBIND) \
    + N(OPERATOR, HOTLOAD_OPERATORS) \
    + vals
action_sugar_binary.set_parse_action(PConst.build_action_component)

action_sugar_unary  = N(OPERATOR, HOTLOAD_OPERATORS) + vals
action_sugar_unary.set_parse_action(PConst.build_action_component)

basic_actions       =  action_component | action_sugar_unary | action_sugar_binary

# Sentences are hinted as using DEFAULT_ACTION, usually assert
action_exprs        = HOTLOAD_ACTION_STATEMENTS | basic_actions | SENTENCE
actions             = IndentedBlock(action_exprs)
actions.set_parse_action(PConst.build_action)

action_definition   = PU.STATEMENT_CONSTRUCTOR(ACTION_HEAD, actions)

# parse action

# NAMING
# vals.set_name("ActionValueList")
# action_component.set_name("ActionComponent")
actions.set_name("Action")
action_definition.set_name("ActionStatement")

# parse_point = actions.ignore(PU.COMMENT)
parse_point = actions

def parse_string(in_string):
    return parse_point.parse_string(in_string)[0][1]
