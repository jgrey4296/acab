""" Trie-based parser to construct rules """
import logging as root_logger
import pyparsing as pp
from acab.working_memory.trie_wm.trie_rule import TrieRule
from acab.abstract.parsing import util as PU
from acab.working_memory.trie_wm import util as WMU
from acab.working_memory.trie_wm.fact_node import FactNode
from acab.abstract.production_operator import ProductionContainer

from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP

logging = root_logger.getLogger(__name__)

# Constructor:
def build_rule(toks):

    # Get Conditions
    if WMU.QUERY_S in toks:
        c = toks[WMU.QUERY_S][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if WMU.TRANSFORM_S in toks:
        t = toks[WMU.TRANSFORM_S][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if WMU.ACTION_S in toks:
        a = toks[WMU.ACTION_S][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None


    rule = TrieRule(c, action=a, transform=t)
    return (rule.type, rule)


conditions = PU.N(WMU.QUERY_S, QP.clauses + PU.gap)
transforms = PU.N(WMU.TRANSFORM_S, TP.transforms + PU.gap)
actions    = PU.NG(WMU.ACTION_S, AP.actions + PU.component_gap)

rule_body = PU.op(conditions) + PU.op(transforms) + PU.op(actions)

rule = PU.STATEMENT_CONSTRUCTOR(PU.RULE_HEAD,
                                FP.BASIC_SEN,
                                rule_body,
                                args=False)

rules = pp.delimitedList(rule, delim=PU.emptyLine)

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
