""" A Trie based Parser module for the creation of action """
import logging as root_logger

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import DELIM, NG, N, component_gap, orm, zrm
from acab.core.parsing.default_structure import OPERATOR
from acab.core.parsing.default_symbols import ACTION_HEAD
from acab.core.parsing.parsers import VALBIND
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.util import LEFT_S, RIGHT_S
from acab.core.parsing.indented_block import IndentedBlock

from .FactParser import SENTENCE, op_sentence

logging = root_logger.getLogger(__name__)

HOTLOAD_OPERATORS         = pp.Forward()
HOTLOAD_ACTION_STATEMENTS = pp.Forward()

# fact string with the option of binds
vals = PU.zrm(SENTENCE)(RIGHT_S)

# action: [op](values)
action_component    = N(OPERATOR, op_sentence) + vals
action_component.setParseAction(PConst.build_action_component)

action_sugar_binary = N(LEFT_S, VALBIND) \
    + N(OPERATOR, HOTLOAD_OPERATORS) \
    + vals
action_sugar_binary.setParseAction(PConst.build_action_component)

action_sugar_unary  = N(OPERATOR, HOTLOAD_OPERATORS) + vals
action_sugar_unary.setParseAction(PConst.build_action_component)

basic_actions       =  action_component | action_sugar_unary | action_sugar_binary

# Sentences are hinted as using DEFAULT_ACTION, usually assert
action_exprs        = HOTLOAD_ACTION_STATEMENTS | basic_actions | SENTENCE
actions             = IndentedBlock(action_exprs)
actions.setParseAction(PConst.build_action)

action_definition   = PU.STATEMENT_CONSTRUCTOR(pp.Literal("::α"), actions)

# parse action

# NAMING
# vals.setName("ActionValueList")
# action_component.setName("ActionComponent")
actions.setName("Action")
action_definition.setName("ActionStatement")

# parse_point = actions.ignore(PU.COMMENT)
parse_point = actions

def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
