""" A Trie based Parser module for the creation of action """
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import DELIM, NG, N, component_gap, orm, zrm
from acab.abstract.parsing.default_structure import OPERATOR
from acab.abstract.parsing.default_symbols import ACTION_HEAD
from acab.abstract.parsing.parsers import VALBIND
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.util import LEFT_S, RIGHT_S
from acab.abstract.parsing.indented_block import IndentedBlock

from .FactParser import BASIC_SEN, PARAM_SEN, op_path

logging = root_logger.getLogger(__name__)

HOTLOAD_OPERATORS = pp.Forward()

# fact string with the option of binds
vals = PU.zrm(PARAM_SEN)(RIGHT_S)

# action: [op](values)
action_component = N(OPERATOR, op_path) + vals
action_component.setParseAction(PConst.build_action_component)

action_sugar = N(LEFT_S, VALBIND) \
    + N(OPERATOR, HOTLOAD_OPERATORS) \
    + vals

action_exprs = action_component | PARAM_SEN | action_sugar
# Sentences are asserted by default
actions = IndentedBlock(action_exprs)
actions.setParseAction(PConst.build_action)

action_definition = PU.STATEMENT_CONSTRUCTOR(BASIC_SEN, actions)

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
