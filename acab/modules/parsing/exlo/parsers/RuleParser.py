""" Trie-based parser to construct rules """
# pylint: disable=bad-whitespace
import logging as root_logger
import pyparsing as pp

from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing.consts import ARROW, DOUBLEBAR, COLON, COMMA, COLON, DELIM, component_gap
from acab.abstract.parsing.consts import RULE_HEAD, N, NG, orm, op, gap, component_gap, emptyLine
from acab.abstract.parsing.consts import QUERY_S, TRANSFORM_S, ACTION_S
from acab.abstract.parsing.funcs import build_rule

from acab.abstract.config.config import AcabConfig

from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP

logging = root_logger.getLogger(__name__)

conditions = N(QUERY_S,     QP.clauses + gap)
transforms = N(TRANSFORM_S, TP.transforms + gap)
actions    = N(ACTION_S,    AP.actions + component_gap)

rule_body = op(conditions) + op(transforms) + op(actions)

rule = PU.STATEMENT_CONSTRUCTOR(RULE_HEAD,
                                FP.BASIC_SEN,
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
