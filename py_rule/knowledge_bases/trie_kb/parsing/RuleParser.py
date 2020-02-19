""" Trie-based parser to construct rules """
import logging as root_logger
import pyparsing as pp
from py_rule.knowledge_bases.trie_kb.trie_rule import TrieRule
from py_rule.abstract.parsing import util as PU
from py_rule.knowledge_bases.trie_kb import util as kb_util

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
    name = toks[kb_util.RULE_NAME_S][0]
    if kb_util.CONDITION_S in toks:
        c = toks[kb_util.CONDITION_S][0]
    else:
        c = None
    if kb_util.TRANSFORM_S in toks:
        t = toks[kb_util.TRANSFORM_S][0]
    else:
        t = None
    if kb_util.ACTION_S in toks:
        a = toks[kb_util.ACTION_S][:]
    else:
        a = []
    if kb_util.TAG_S in toks:
        tags = [x[1] for x in toks[kb_util.TAG_S]]
    else:
        tags = []
    return TrieRule(c, a, transform=t, name=name, tags=tags)


ruleName = FP.NG(kb_util.RULE_NAME_S, FP.param_fact_string)

tagName = PU.HASH + FP.NAME

tagList = PU.N(kb_util.TAG_S, tagName
               + pp.ZeroOrMore(PU.COMMA + tagName)
               + PU.emptyLine)
conditions = PU.N(kb_util.CONDITION_S, QP.clauses + PU.emptyLine)
transforms = PU.N(kb_util.TRANSFORM_S, TP.transforms + PU.emptyLine)
actions = PU.N(kb_util.ACTION_S, AP.actions + PU.emptyLine)

rule = ruleName + PU.COLON + PU.sLn \
       + PU.op(tagList) \
       + PU.op(conditions) + PU.op(transforms) + PU.op(actions) \
       + PU.end

rules = rule + pp.ZeroOrMore(PU.emptyLine + rule)

# Actions:
rule.setParseAction(build_rule)


# Main Parser
def parseString(in_string):
    assert(isinstance(in_string, str))
    return rules.parseString(in_string)[:]
