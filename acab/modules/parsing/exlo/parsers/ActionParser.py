""" A Trie based Parser module for the creation of action """
##-- imports
from __future__ import annotations
import logging as logmod

import pyparsing as pp
import acab
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (DELIM, NG, N, component_gap, ln, op, orm,
                                      s, zrm)
from acab.core.defaults.parse_keys import OPERATOR
from acab.core.parsing.parsers import VALBIND
from acab.core.parsing.pyparse_ext.statement_core import StatementCore
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.util import LEFT_S, RIGHT_S, ACTION_COMPONENT

from .FactParser import SENTENCE, op_sentence

##-- end imports

logging = logmod.getLogger(__name__)

HOTLOAD_OPERATORS         = pp.Forward()
HOTLOAD_OPERATORS.set_name("hl_action_operators")
HOTLOAD_ACTION_STATEMENTS = pp.Forward()
HOTLOAD_ACTION_STATEMENTS.set_name("hl_action_statements")

# fact string with the option of binds
vals = PU.zrm(SENTENCE)(RIGHT_S)

# action: [op](values)
action_component    = N(OPERATOR, op_sentence) + vals
action_component.set_name("action_component")
action_component.set_parse_action(PConst.build_action_component)

action_sugar_binary = (N(LEFT_S, VALBIND)
                       + NG(OPERATOR, HOTLOAD_OPERATORS)
                       + vals)
action_sugar_binary.set_parse_action(PConst.build_action_component)

action_sugar_unary  = NG(OPERATOR, HOTLOAD_OPERATORS) + vals
action_sugar_unary.set_parse_action(PConst.build_action_component)

basic_actions       =  action_component | action_sugar_unary | action_sugar_binary
basic_actions.set_name("basic_actions")

# Sentences are hinted as using DEFAULT_ACTION, usually assert
action_exprs        = HOTLOAD_ACTION_STATEMENTS | basic_actions | SENTENCE
actions             = pp.IndentedBlock(action_exprs + op(s(ln)))
actions.set_parse_action(PConst.build_action)

action_definition   = StatementCore(ACTION_COMPONENT, actions)

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
