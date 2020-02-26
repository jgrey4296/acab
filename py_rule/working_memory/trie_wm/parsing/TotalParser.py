""""
A Combined parser to parse rules and facts to assert
Handles files, and comments
"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc

from py_rule.abstract.sentence import Sentence
from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.action import ActionMacro
from py_rule.abstract.rule import Rule
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.abstract.parsing import util as PU
from py_rule.error.pyrule_parse_exception import PyRuleParseException

from . import FactParser as FP
from . import RuleParser as RP
from . import ActionParser as AP

# Temporary state of a file parse
parseBindings = {}

OTHER_STATEMENTS = pp.Forward()

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
        assertions.append(x)
        if isinstance(x[-1]._value, Rule):
            rules.append(x)
        elif isinstance(x[-1]._value, ActionMacro):
            action_macros[x._name] = x

    #everything has been parsed,
    #clear the parse bindings as a guard:
    parseBindings = {}
    # TODO expand rulemacros into rule sequences:
    expandedActMacroRules = [x[-1]._value.expand_action_macros(action_macros) for x in rules]
    return (expandedActMacroRules, assertions)

def add_file_binding(toks):
    """ Store the string of in the binding """
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
    #or if a rule or action macro:
    elif isinstance(toks[0], (Rule, ActionMacro)):
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

def trie_wrap_statement_into_sentence(toks):
    """ Take a parsed statement (eg: rule),
    and add it as a node to the name sentence
    that locates it
    """
    type_name, value = toks[0]
    assert(isinstance(value, PyRuleValue))
    loc = value._name.copy()
    assert(isinstance(loc, Sentence))
    node_data = WMU.DEFAULT_NODE_DATA.copy()
    node_data[WMU.VALUE_TYPE_S : type_name]
    node_data[WMU.OPERATOR_S : WMU.EXOP.EX]

    loc.add(FactNode(value, node_data))

    return loc


statement_converter = pp.Or([RP.rule, OTHER_STATEMENTS])

bindArrow = PU.s(pp.Literal('<-'))
clear = PU.s(pp.Literal('clear'))
fileBind = FP.VALBIND + bindArrow + FP.PARAM_SEN
clearBind = clear + PU.orm(FP.VALBIND)

file_component = pp.MatchFirst([PU.s(fileBind),
                                PU.s(clearBind),
                                AP.action_definition,
                                statement_converter,
                                FP.PARAM_SEN])

file_total = file_component \
    + pp.ZeroOrMore(PU.s(PU.orm(pp.Or([PU.COMMA, pp.lineEnd]))) + file_component)

#Parse Actions
file_total.setParseAction(final_pass)
fileBind.setParseAction(add_file_binding)
clearBind.setParseAction(clearBinding)
file_component.setParseAction(expansion_pass)
statement_converter.setParseAction(trie_wrap_statement_into_sentence)


def parseString(in_string):
    assert(isinstance(in_string, str))
    no_comments = remove_comments(in_string)
    return file_total.parseString(no_comments)[0]
