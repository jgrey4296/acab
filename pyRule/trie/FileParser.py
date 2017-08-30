""""
A Combined parser to parse rules and facts to assert

"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc
from pyRule.utils import Bind, expandFact
from pyRule.trie import Rule
import IPython

from . import FactParser as FP
from . import RuleParser as RP
from .Rule import Rule

parseBindings = {}

def split_into_rules_and_assertions(toks):
    rules = []
    assertions = []
    for x in toks:
        if isinstance(x, Rule):
            rules.append(x)
        else:
            assertions.append(x)
    return (rules, assertions)

def add_file_binding(toks):
    """ Store the string of in the binding """
    binding = toks[0]
    string = toks[1]
    assert(isinstance(binding, Bind))
    assert(isinstance(string, list))
    assert(binding.value not in parseBindings)
    parseBindings[binding.value] = string
    return []

def expandBinding(toks):
    """ Expand any bindings that are stored in the parse """
    if len(toks) == 0:
        return toks
    elif len(toks) > 1:
        raise Exception("Unexpected toks size for binding expansion")
    #if a fact:
    if isinstance(toks[0], list):
        return [expandFact(toks[0], parseBindings)]
    #or if a rule:
    elif isinstance(toks[0], Rule):
        return [toks[0].expandBindings(parseBindings)]
    return toks

def clearBinding(toks):
    assert(all([isinstance(x, Bind) for x in toks]))
    assert(all([x.value in parseBindings for x in toks]))
    for x in toks:
        del parseBindings[x.value]
    return []

pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
orm = pp.OneOrMore
comment = pp.dblSlashComment
bindArrow = s(pp.Literal('<-'))
clear = s(pp.Literal('clear'))

fileBind = FP.BIND + bindArrow + FP.param_fact_string
clearBind = clear + pp.OneOrMore(FP.BIND)




rule_or_assertion = pp.MatchFirst([s(comment),
                                   s(fileBind),
                                   s(clearBind),
                                   RP.rule,
                                   FP.param_fact_string])

manyRorA = rule_or_assertion + pp.ZeroOrMore(s(orm(pp.lineEnd)) + rule_or_assertion)

#Parse Actions
manyRorA.setParseAction(split_into_rules_and_assertions)
fileBind.setParseAction(add_file_binding)
clearBind.setParseAction(clearBinding)
rule_or_assertion.setParseAction(expandBinding)

def parseString(s):
    assert(isinstance(s, str))
    return manyRorA.parseString(s)[0]
 



