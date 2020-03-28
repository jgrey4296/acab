""" Trie-based parser to construct rules """
import logging as root_logger
import pyparsing as pp
from py_rule.working_memory.trie_wm.trie_rule import TrieRule
from py_rule.abstract.parsing import util as PU
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.working_memory.trie_wm.nodes.fact_node import FactNode
from py_rule.abstract.production_operator import ProductionContainer

from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP

logging = root_logger.getLogger(__name__)


# Hotloader:
def build_operators():
    QP.build_operators()
    TP.build_operators()
    AP.build_operators()


# Constructor:
def build_rule(toks):

    # Get Conditions
    if WMU.CONDITION_S in toks:
        c = toks[WMU.CONDITION_S][0][1]
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

    # Get Tags
    if WMU.TAG_S in toks:
        tags = [x[1] for x in toks[WMU.TAG_S]]
    else:
        tags = []


    rule = TrieRule(c, action=a, transform=t, tags=tags)
    return (rule._type, rule)


tagName = PU.HASH + PU.NAME

tagList    = PU.N(WMU.TAG_S, pp.delimitedList(tagName, delim=PU.DELIM) + PU.emptyLine)
conditions = PU.N(WMU.CONDITION_S, QP.clauses + PU.emptyLine)
transforms = PU.N(WMU.TRANSFORM_S, TP.transforms + PU.emptyLine)
actions    = PU.NG(WMU.ACTION_S, AP.actions + PU.emptyLine)

rule_body = PU.op(tagList) + PU.op(conditions) + PU.op(transforms) + PU.op(actions)

rule = PU.STATEMENT_CONSTRUCTOR(PU.RULE_HEAD, FP.BASIC_SEN, rule_body, args=False)

rules = pp.delimitedList(rule, delim=PU.emptyLine)

# Actions:
rule_body.setParseAction(build_rule)

# Main Parser
def parseString(in_string):
    assert(isinstance(in_string, str))
    return rules.parseString(in_string)[:]
