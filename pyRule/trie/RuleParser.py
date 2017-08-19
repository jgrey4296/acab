import logging as root_logger
import pyparsing as pp
from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP

pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

logging = root_logger.getLogger(__name__)

s = pp.Suppress
op = pp.Optional
sLn = s(pp.White(ws='\n',exact=1))
opLn = s(op(pp.LineEnd()))
COLON = s(pp.Literal(':'))
emptyLine = s(pp.lineEnd + pp.lineEnd)
ruleEnd = s(pp.Literal('end'))

ruleName = FP.param_fact_string.copy().setResultsName('ruleName')

conditions = (QP.clauses + emptyLine).setResultsName('conditions')
transforms = (TP.transforms + emptyLine).setResultsName('transforms')
actions = (AP.actions + emptyLine).setResultsName('actions')

rule = ruleName + COLON + sLn \
       + op(conditions) + op(transforms) + op(actions) \
       + ruleEnd


#todo: a parser to combine { query -> transform -> action }
"""
.rule.test:
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

def parseString(s):
    return s

def parseStrings(s):
    return s


"""
Rules as Fact Strings, so:
.a.rule.?.a.b.$x(>20)!$z(!=x)
.a.rule.?.~.a.b.c
.a.rule.~>.($x + 20 -> $y, $z * 2)
.a.rule.=>(+.a.b.$x, -.a.b.$z, print($z))


"""
