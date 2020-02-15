""""
A Combined parser to parse rules and facts to assert

"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc

from py_rule.abstract.sentence import Sentence
from py_rule.abstract.action import ActionMacro
from py_rule.abstract.rule import Rule
from py_rule.typing.ex_types import TypeDefinition
import py_rule.utils as utils

from . import FactParser as FP
from . import RuleParser as RP
from . import ActionParser as AP
from . import TypeDefParser as TDP

comment = pp.dblSlashComment

parseBindings = {}

def final_pass(toks):
    """ The final action of the file parser.
    splits out rules and assertions,
    and expands action macros in rules into the actual
    action sequences """
    global parseBindings
    definitions = []
    rules = []
    assertions = []
    action_macros = {}
    for x in toks:
        if isinstance(x, TypeDefinition):
            definitions.append(x)
        elif isinstance(x, Rule):
            rules.append(x)
        elif isinstance(x, ActionMacro):
            action_macros[x._name] = x
        else:
            assertions.append(x)
    #everything has been parsed,
    #clear the parse bindings as a guard:
    parseBindings = {}
    #and expand rulemacros into rule sequences:
    expandedActMacroRules = [x.expand_action_macros(action_macros) for x in rules]
    return (definitions, expandedActMacroRules, assertions)

def add_file_binding(toks):
    """ Store the string of in the binding """
    binding = toks[0]
    string = toks[1]
    assert(binding._data[utils.BIND_S])
    assert(isinstance(string, Sentence))
    assert(binding._value not in parseBindings)
    parseBindings[binding._value] = string
    return []

def expansion_pass(toks):
    """ Expand any bindings that are stored in the parse """
    if not bool(toks):
        return toks
    elif len(toks) > 1:
        raise Exception("Unexpected toks size for binding expansion")
    #if a fact:
    if isinstance(toks[0], Sentence):
        return [toks[0].expand_bindings(parseBindings)]
    #or if a rule or action macro:
    elif isinstance(toks[0], (Rule, ActionMacro)):
        return [toks[0].expand_bindings(parseBindings)]
    return toks

def clearBinding(toks):
    assert(all([x._data[utils.BIND_S] for x in toks]))
    assert(all([x._value in parseBindings for x in toks]))
    for x in toks:
        del parseBindings[x._value]
    return []

def remove_comments(string):
    lines = string.split("\n")
    passing_lines = []
    for line in lines:
        passing_lines.append("".join(list(comment.split(line))))
    return "\n".join(passing_lines).strip()



pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
orm = pp.OneOrMore
bindArrow = s(pp.Literal('<-'))
clear = s(pp.Literal('clear'))
fileBind = FP.VALBIND + bindArrow + FP.param_fact_string
clearBind = clear + orm(FP.VALBIND)


file_component = pp.MatchFirst([s(fileBind),
                                s(clearBind),
                                AP.action_definition,
                                RP.rule,
                                TDP.TYPEDEF,
                                FP.param_fact_string])

file_total = file_component + pp.ZeroOrMore(s(orm(pp.lineEnd)) + file_component)

#Parse Actions
file_total.setParseAction(final_pass)
fileBind.setParseAction(add_file_binding)
clearBind.setParseAction(clearBinding)
file_component.setParseAction(expansion_pass)




def parseString(in_string):
    assert(isinstance(in_string, str))
    no_comments = remove_comments(in_string)
    return file_total.parseString(no_comments)[0]
