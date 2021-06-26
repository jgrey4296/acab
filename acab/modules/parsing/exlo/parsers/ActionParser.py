""" A Trie based Parser module for the creation of action """
import logging as root_logger
import pyparsing as pp

from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.default_structure import OPERATOR
from acab.abstract.parsing.default_symbols import ACTION_HEAD
from acab.abstract.parsing.consts import N, NG, zrm, orm, DELIM, component_gap
from acab.modules.parsing.exlo.util import RIGHT_S, LEFT_S
from acab.abstract.parsing.parsers import VALBIND
from acab.abstract.parsing import funcs as Pfunc
from acab.modulies.parsing.exlo import constructors as PConst


from acab.abstract.config.config import AcabConfig

from .FactParser import PARAM_SEN, BASIC_SEN, PARAM_SEN, op_path

logging = root_logger.getLogger(__name__)

HOTLOAD_OPERATORS = pp.Forward()


# fact string with the option of binds
vals = N(RIGHT_S, PU.zrm(PARAM_SEN))

# action: [op](values)
action_component = N(OPERATOR, op_path) + vals

action_sugar = N(LEFT_S, VALBIND) \
    + N(OPERATOR, HOTLOAD_OPERATORS) \
    + vals

# Sentences are asserted by default
actions = pp.delimitedList(pp.Or([action_component, PARAM_SEN]), delim=DELIM)

action_definition = PU.STATEMENT_CONSTRUCTOR(ACTION_HEAD,
                                             BASIC_SEN,
                                             actions + component_gap)

# parse action
action_component.setParseAction(PConst.build_action_component)
actions.setParseAction(PConst.build_action)

# NAMING
vals.setName("ActionValueList")
action_component.setName("ActionComponent")
actions.setName("ActionsContainer")
action_definition.setName("ActionDefinition")

# parse_point = actions.ignore(PU.COMMENT)
parse_point = actions

def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
