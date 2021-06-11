""""
A Combined parser to parse rules and facts to assert
Handles files, and comments
"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc

from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import ARROW, DOUBLEBAR, COLON, COMMA, COLON, DELIM, component_gap
from acab.abstract.parsing.consts import file_cruft, N, NG, COMMENT

from . import FactParser as FP
from . import RuleParser as RP
from . import ActionParser as AP

HOTLOAD_STATEMENTS = pp.Forward()

statements = pp.Or([RP.rule,
                    AP.action_definition,
                    FP.SEN_STATEMENT,
                    HOTLOAD_STATEMENTS])

file_component = pp.Or([statements, FP.PARAM_SEN])

file_total = pp.delimitedList(file_component, delim=component_gap)

# NAMING
HOTLOAD_STATEMENTS.setName("HotloadStatement")
statements.setName("StatementCollection")
file_component.setName("FileComponent")

parse_point = file_cruft +  file_total.ignore(COMMENT) + file_cruft
# parse_point = file_cruft +  file_total + file_cruft

def parseString(in_string, parse_point=parse_point):
    assert(isinstance(in_string, str))
    return parse_point.parseString(in_string)[:]

def parseFile(f, parse_point=parse_point):
    return parse_point.parseFile(f, parseAll=True)
