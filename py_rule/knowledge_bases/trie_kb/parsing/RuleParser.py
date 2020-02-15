""" Trie-based parser to construct rules """
import logging as root_logger
import pyparsing as pp
from py_rule.knowledge_bases.trie_kb.trie_rule import TrieRule

from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP

pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

logging = root_logger.getLogger(__name__)

def build_rule(toks):
    name = toks.rulename[0]
    if 'conditions' in toks:
        c = toks.conditions[0]
    else:
        c = None
    if 'transforms' in toks:
        t = toks.transforms[0]
    else:
        t = None
    if 'actions' in toks:
        a = toks.actions[:]
    else:
        a = []
    if 'tags' in toks:
        tags = [x[1] for x in toks.tags]
    else:
        tags = []
    return TrieRule(c, a, transform=t, name=name, tags=tags)


s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
HASH = s(pp.Literal('#'))
emptyLine = s(pp.OneOrMore(pp.lineEnd))

ruleName = FP.NG("rulename", FP.param_fact_string)
tagName = HASH + FP.NAME

tagList = FP.N("tags", tagName + pp.ZeroOrMore(FP.COMMA + tagName) + emptyLine)
conditions = FP.N("conditions", QP.clauses + emptyLine)
transforms = FP.N("transforms", TP.transforms + emptyLine)
actions = FP.N("actions", AP.actions + emptyLine)

rule = ruleName + FP.COLON + FP.sLn \
       + op(tagList) \
       + op(conditions) + op(transforms) + op(actions) \
       + FP.end

rules = rule + pp.ZeroOrMore(emptyLine + rule)

rule.setParseAction(build_rule)

def parseString(in_string):
    assert(isinstance(in_string, str))
    return rules.parseString(in_string)[:]


"""
meta rules:
add/remove/replace penumbra conditions
modify penumbra transforms
add/remove/replace penumbra actions
"""
