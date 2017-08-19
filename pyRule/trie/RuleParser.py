import logging as root_logger
import pyparsing as pp
from .Rule import Rule
from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP


import IPython
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

logging = root_logger.getLogger(__name__)

def build_rule(toks):
    name = toks.ruleName[:]
    if 'conditions' in toks:
        c = toks.conditions[0]
    else:
        c = []
    if 'transforms' in toks:
        t = toks.transforms[0]
    else:
        t = None
    if 'actions' in toks:
        a = toks.actions[:]
    else:
        a = []
    if 'tags' in toks:
        tags = toks.tags[:]
    else:
        tags = []
    return Rule(c, a, transform=t, name=name, tags=tags)
        
        
s = pp.Suppress
op = pp.Optional
sLn = s(pp.White(ws='\n',exact=1))
opLn = s(op(pp.LineEnd()))
COLON = s(pp.Literal(':'))
HASH = s(pp.Literal('#'))
emptyLine = s(pp.lineEnd + pp.lineEnd)
ruleEnd = s(pp.Literal('end'))

ruleName = FP.param_fact_string.copy().setResultsName('ruleName')
tagName = HASH + FP.NAME

tagList = (tagName + pp.ZeroOrMore(FP.COMMA + tagName) + emptyLine).setResultsName('tags')
conditions = (QP.clauses + emptyLine).setResultsName('conditions')
transforms = (TP.transforms + emptyLine).setResultsName('transforms')
actions = (AP.actions + emptyLine).setResultsName('actions')

rule = ruleName + COLON + sLn \
       + op(tagList) \
       + op(conditions) + op(transforms) + op(actions) \
       + ruleEnd

rule.setParseAction(build_rule)

rules = rule + pp.ZeroOrMore(emptyLine + rule)

def parseString(s):
    assert(isinstance(s, str))
    return rules.parseString(s)[:]


"""
todo: a parser to combine { query -> transform -> action }

.rule.test:
	#tags

	 .a.b.c.d!20
	 .a.d.e.$t(>30)

         ($t + 20 -> $y)

         +.a.b.c.d
          +.a.b.e!$t
          -.a.b.e.f
          @"Blah"
          something("else")
end

meta rules:
add/remove/replace penumbra conditions
modify penumbra transforms
add/remove/replace penumbra actions
"""
