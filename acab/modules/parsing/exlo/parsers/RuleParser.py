""" Trie-based parser to construct rules """
# pylint: disable=bad-whitespace
import logging as root_logger

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (ARROW, COLON, COMMA, DELIM,
                                          DOUBLEBAR, NG, RULE_HEAD, N,
                                          component_gap, emptyLine, gap, op,
                                          orm, END)
from acab.modules.parsing.exlo.util import ACTION_S, QUERY_S, TRANSFORM_S, RULE_HEAD
from acab.modules.parsing.exlo.constructors import build_rule

from . import ActionParser as AP
from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP

logging = root_logger.getLogger(__name__)

# all of these should be indented blocks
conditions = N(QUERY_S,     QP.clauses)
transforms = N(TRANSFORM_S, TP.transforms)
actions    = N(ACTION_S,    AP.actions)

endOrLine  = pp.FollowedBy(END) | emptyLine | pp.string_end
# endOrLine.leave_whitespace()

rule_body = op(conditions + endOrLine) + op(transforms + endOrLine) + op(actions + endOrLine)

rule = PU.STATEMENT_CONSTRUCTOR(RULE_HEAD, rule_body)


# Actions:
rule_body.set_parse_action(build_rule)

# NAMING
# conditions.set_name("RuleConditions")
# transforms.set_name("RuleTransforms")
# actions.set_name("RuleActions")
rule_body.set_name("RuleBody")
rule.set_name("RuleDefinition")
endOrLine.set_name("endOrLine")
# rules.set_name("RulePlural")


# parse_point = rules.ignore(PU.COMMENT)

# Main Parser
parse_point = rule

# Main Parser
def parse_string(in_string):
    assert(isinstance(in_string, str))
    return parse_point.parse_string(in_string)[:]
