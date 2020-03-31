""""
A Combined parser to parse rules and facts to assert
Handles files, and comments
"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc

from py_rule.abstract.sentence import Sentence
from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.rule import Rule
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.abstract.parsing import util as PU
from py_rule.error.pyrule_parse_exception import PyRuleParseException

from . import FactParser as FP
from . import RuleParser as RP
from . import ActionParser as AP

# Temporary state of a file parse
parseBindings = {}

HOTLOAD_STATEMENTS = pp.Forward()

def final_pass(toks):
    """ The final action of the file parser.
    splits out rules and assertions,
    and expands action macros in rules into the actual
    action sequences """
    global parseBindings

    #everything has been parsed,
    #clear the parse bindings as a guard:
    parseBindings = {}
    return toks[:]

def add_file_binding(toks):
    """ Store the string in the binding """
    binding = toks[0]
    string = toks[1]
    assert(binding._data[WMU.BIND_S])
    assert(isinstance(string, Sentence))
    assert(binding._value not in parseBindings)
    parseBindings[binding._value] = string
    return []

def expansion_pass(toks):
    """ Expand any bindings that are stored in the parse """
    if not bool(toks):
        return toks
    elif len(toks) > 1:
        raise PyRuleParseException("Unexpected toks size for binding expansion")
    #if a fact:
    if isinstance(toks[0], Sentence):
        return [toks[0].expand_bindings(parseBindings)]
    return toks

def clearBinding(toks):
    assert(all([x._data[WMU.BIND_S] for x in toks]))
    assert(all([x._value in parseBindings for x in toks]))
    for x in toks:
        del parseBindings[x._value]
    return []

def remove_comments(string):
    lines = string.split("\n")
    passing_lines = []
    for line in lines:
        passing_lines.append("".join(list(PU.COMMENT.split(line))))
    return "\n".join(passing_lines).strip()


statements = pp.Or([RP.rule, AP.action_definition, HOTLOAD_STATEMENTS])

bindArrow = PU.s(pp.Literal('<-'))
clear = PU.s(pp.Literal('clear'))
fileBind = FP.VALBIND + bindArrow + FP.PARAM_SEN
clearBind = clear + PU.orm(FP.VALBIND)

file_component = pp.MatchFirst([PU.s(fileBind),
                                PU.s(clearBind),
                                statements,
                                FP.PARAM_SEN])

file_delim = pp.ZeroOrMore(pp.lineEnd)

file_total = file_delim + pp.delimitedList(file_component, delim=file_delim)

#Parse Actions
file_total.setParseAction(final_pass)
fileBind.setParseAction(add_file_binding)
clearBind.setParseAction(clearBinding)
file_component.setParseAction(expansion_pass)

# NAMING
statements.setName("StatementCollection")
file_component.setName("FileComponent")

parse_point = PU.file_cruft +  file_total.ignore(PU.COMMENT) + PU.file_cruft
# parse_point = PU.file_cruft +  file_total + PU.file_cruft

def parseString(in_string):
    assert(isinstance(in_string, str))
    return parse_point.parseString(in_string)[:]
