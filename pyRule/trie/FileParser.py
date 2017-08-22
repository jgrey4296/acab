""""
A Combined parser to parse rules and facts to assert

"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc
import IPython

from . import FactParser as FP
from . import RuleParser as RP
from .Rule import Rule

def split_into_rules_and_assertions(toks):
    rules = []
    assertions = []
    for x in toks:
        if isinstance(x, Rule):
            rules.append(x)
        else:
            assertions.append(x)
    return (rules, assertions)


pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
orm = pp.OneOrMore
comment = pp.dblSlashComment

rule_or_assertion = pp.MatchFirst([s(comment), RP.rule, FP.param_fact_string])

manyRorA = rule_or_assertion + pp.ZeroOrMore(s(orm(pp.lineEnd)) + rule_or_assertion)

manyRorA.setParseAction(split_into_rules_and_assertions)

def parseString(s):
    assert(isinstance(s, str))
    return manyRorA.parseString(s)[0]
 



