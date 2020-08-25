""""
A Combined parser to parse rules and facts to assert
Handles files, and comments
"""
import pyparsing as pp
from pyparsing import pyparsing_common as ppc

from acab.abstract.sentence import Sentence
from acab.abstract.value import AcabValue
from acab.abstract.rule import Rule
from acab.abstract.parsing import util as PU
from acab.error.acab_parse_exception import AcabParseException

from . import FactParser as FP
from . import RuleParser as RP
from . import ActionParser as AP


HOTLOAD_STATEMENTS = pp.Forward()

statements = pp.Or([RP.rule,
                    AP.action_definition,
                    FP.SEN_STATEMENT,
                    HOTLOAD_STATEMENTS])

file_component = pp.Or([statements, FP.PARAM_SEN])

file_total = pp.delimitedList(file_component, delim=PU.component_gap)

# NAMING
HOTLOAD_STATEMENTS.setName("HotloadStatement")
statements.setName("StatementCollection")
file_component.setName("FileComponent")

parse_point = PU.file_cruft +  file_total.ignore(PU.COMMENT) + PU.file_cruft
# parse_point = PU.file_cruft +  file_total + PU.file_cruft

def parseString(in_string):
    assert(isinstance(in_string, str))
    return parse_point.parseString(in_string)[:]

def parseFile(f):
    return parse_point.parseFile(f, parseAll=True)
