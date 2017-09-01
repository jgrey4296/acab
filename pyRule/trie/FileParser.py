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
from . import ActionParser as AP
from .Rule import Rule
from pyRule.Actions import ActionMacro


parseBindings = {}

def final_pass(toks):
    """ The final action of the file parser.
    splits out rules and assertions, 
    and expands action macros in rules into the actual
    action sequences """
    rules = []
    assertions = []
    action_macros = {}
    for x in toks:
        if isinstance(x, Rule):
            rules.append(x)
        elif isinstance(x, ActionMacro):
            action_macros[x._name] = x
        else:
            assertions.append(x)
    #everything has been parsed,
    #clear the parse bindings as a guard:
    parseBindings = {}
    #and expand rulemacros into rule sequences:
    expandedActMacroRules = [x.expandActionMacros(action_macros) for x in rules]    
    return (expandedActMacroRules, assertions)

def add_file_binding(toks):
    """ Store the string of in the binding """
    binding = toks[0]
    string = toks[1]
    assert(isinstance(binding, Bind))
    assert(isinstance(string, list))
    assert(binding.value not in parseBindings)
    parseBindings[binding.value] = string
    return []

def expansion_pass(toks):
    """ Expand any bindings that are stored in the parse """
    if len(toks) == 0:
        return toks
    elif len(toks) > 1:
        raise Exception("Unexpected toks size for binding expansion")
    #if a fact:
    if isinstance(toks[0], list):
        return [expandFact(toks[0], parseBindings)]
    #or if a rule or action macro:
    elif isinstance(toks[0], (Rule, ActionMacro)):
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
clearBind = clear + orm(FP.BIND)




file_component = pp.MatchFirst([s(comment),
                                s(fileBind),
                                s(clearBind),
                                AP.action_definition,
                                RP.rule,
                                FP.param_fact_string])

file_total = file_component + pp.ZeroOrMore(s(orm(pp.lineEnd)) + file_component)

#Parse Actions
file_total.setParseAction(final_pass)
fileBind.setParseAction(add_file_binding)
clearBind.setParseAction(clearBinding)
file_component.setParseAction(expansion_pass)

def parseString(s):
    assert(isinstance(s, str))
    return file_total.parseString(s)[0]
 



