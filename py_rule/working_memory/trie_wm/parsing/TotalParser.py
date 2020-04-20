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


HOTLOAD_STATEMENTS = pp.Forward()

def final_pass(toks):
    #everything has been parsed,
    #clear the parse bindings as a guard:
    return toks[:]

statements = pp.Or([RP.rule,
                    AP.action_definition,
                    FP.SEN_STATEMENT,
                    HOTLOAD_STATEMENTS])

file_component = pp.MatchFirst([statements, FP.PARAM_SEN])

file_total = pp.delimitedList(file_component, delim=PU.component_gap)

#Parse Actions
file_total.setParseAction(final_pass)

# NAMING
statements.setName("StatementCollection")
file_component.setName("FileComponent")

parse_point = PU.file_cruft +  file_total.ignore(PU.COMMENT) + PU.file_cruft
# parse_point = PU.file_cruft +  file_total + PU.file_cruft

def parseString(in_string):
    assert(isinstance(in_string, str))
    return parse_point.parseString(in_string)[:]
