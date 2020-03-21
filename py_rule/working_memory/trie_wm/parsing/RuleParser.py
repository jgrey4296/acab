""" Trie-based parser to construct rules """
import logging as root_logger
import pyparsing as pp
from py_rule.working_memory.trie_wm.trie_rule import TrieRule
from py_rule.abstract.parsing import util as PU
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.working_memory.trie_wm.nodes.fact_node import FactNode

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
    name = toks[WMU.RULE_NAME_S][0]
    # Get Conditions
    if WMU.CONDITION_S in toks:
        c = toks[WMU.CONDITION_S][0]
    else:
        c = None

    # Get Transform
    if WMU.TRANSFORM_S in toks:
        t = toks[WMU.TRANSFORM_S][0]
    else:
        t = None

    # Get Action
    if WMU.ACTION_S in toks:
        a = toks[WMU.ACTION_S][0]
    else:
        a = None

    # Get Tags
    if WMU.TAG_S in toks:
        tags = [x[1] for x in toks[WMU.TAG_S]]
    else:
        tags = []


    # TODO wrap in sentence
    rule = TrieRule(c, action=a, transform=t, name=name, tags=tags)
    return (WMU.RULE_S, rule)


ruleName = PU.NG(WMU.RULE_NAME_S, FP.PARAM_SEN)

tagName = PU.HASH + PU.NAME

tagList = PU.N(WMU.TAG_S, tagName
               + pp.ZeroOrMore(PU.COMMA + tagName)
               + PU.emptyLine)
conditions = PU.N(WMU.CONDITION_S, QP.clauses + PU.emptyLine)
transforms = PU.N(WMU.TRANSFORM_S, TP.transforms + PU.emptyLine)
actions = PU.N(WMU.ACTION_S, AP.actions + PU.emptyLine)

rule = ruleName + PU.COLON + PU.sLn \
       + PU.op(tagList) \
       + PU.op(conditions) + PU.op(transforms) + PU.op(actions) \
       + PU.end

rules = pp.delimitedList(rule, delim=PU.emptyLine)

# Actions:
rule.setParseAction(build_rule)


# Main Parser
def parseString(in_string):
    assert(isinstance(in_string, str))
    return rules.parseString(in_string)[:]
