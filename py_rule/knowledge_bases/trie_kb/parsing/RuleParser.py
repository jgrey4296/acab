""" Trie-based parser to construct rules """
import logging as root_logger
import pyparsing as pp
from py_rule.knowledge_bases.trie_kb.trie_rule import TrieRule
from py_rule.abstract.parsing import util as PU
from py_rule.knowledge_bases.trie_kb import util as KBU

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
    name = toks[KBU.RULE_NAME_S][0]
    if KBU.CONDITION_S in toks:
        c = toks[KBU.CONDITION_S][0]
    else:
        c = None
    if KBU.TRANSFORM_S in toks:
        t = toks[KBU.TRANSFORM_S][0]
    else:
        t = None
    if KBU.ACTION_S in toks:
        a = toks[KBU.ACTION_S][:]
    else:
        a = []
    if KBU.TAG_S in toks:
        tags = [x[1] for x in toks[KBU.TAG_S]]
    else:
        tags = []
    return TrieRule(c, a, transform=t, name=name, tags=tags)


ruleName = PU.NG(KBU.RULE_NAME_S, FP.PARAM_SEN)

tagName = PU.HASH + PU.NAME

tagList = PU.N(KBU.TAG_S, tagName
               + pp.ZeroOrMore(PU.COMMA + tagName)
               + PU.emptyLine)
conditions = PU.N(KBU.CONDITION_S, QP.clauses + PU.emptyLine)
transforms = PU.N(KBU.TRANSFORM_S, TP.transforms + PU.emptyLine)
actions = PU.N(KBU.ACTION_S, AP.actions + PU.emptyLine)

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
