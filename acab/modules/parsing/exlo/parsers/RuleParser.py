""" Trie-based parser to construct rules """
# pylint: disable=bad-whitespace
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import (ARROW, COLON, COMMA, DELIM,
                                          DOUBLEBAR, NG, RULE_HEAD, N,
                                          component_gap, emptyLine, gap, op,
                                          orm)
from acab.abstract.parsing.default_structure import ACTION, QUERY, TRANSFORM
from acab.modules.parsing.exlo.constructors import build_rule

from . import ActionParser as AP
from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP

logging = root_logger.getLogger(__name__)

conditions = N(QUERY,     QP.clauses + gap)
transforms = N(TRANSFORM, TP.transforms + gap)
actions    = N(ACTION,    AP.actions + component_gap)

rule_body = op(conditions) + op(transforms) + op(actions)

rule = PU.STATEMENT_CONSTRUCTOR(FP.BASIC_SEN,
                                rule_body,
                                args=False)

rules = pp.delimitedList(rule, delim=emptyLine)


# Actions:
rule_body.setParseAction(build_rule)

# NAMING
conditions.setName("RuleConditions")
transforms.setName("RuleTransforms")
actions.setName("RuleActions")
rule_body.setName("RuleBody")
rule.setName("RuleDefinition")
rules.setName("RulePlural")


# parse_point = rules.ignore(PU.COMMENT)

# Main Parser
parse_point = rules

# Main Parser
def parseString(in_string):
    assert(isinstance(in_string, str))
    return parse_point.parseString(in_string)[:]
