""""
A Combined parser to parse rules and facts to assert
Handles files, and comments
"""
import pyparsing as pp
from acab.abstract.parsing.consts import (ARROW, COLON, COMMA, COMMENT, DELIM,
                                          DOUBLEBAR, NG, N, component_gap,
                                          file_cruft)
import acab.abstract.parsing.parsers as PU
from acab.abstract.parsing.funcs import strip_parse_type
from pyparsing import pyparsing_common as ppc

from . import ActionParser as AP
from . import FactParser as FP
from . import QueryParser as QP
from . import RuleParser as RP

HOTLOAD_STATEMENTS = pp.Forward()
# HOTLOAD_STATEMENTS.setName("Statements")

file_component = HOTLOAD_STATEMENTS | FP.SEN_PLURAL

file_total     = PU.DELIMIST(file_component, delim=component_gap)

# NAMING
# HOTLOAD_STATEMENTS.setName("HotloadStatement")
# file_component.setName("FileComponent")

parse_point = (file_cruft + file_total+ file_cruft).ignore(COMMENT)
# parse_point.setName("Total Parser")
# parse_point = file_cruft +  file_total + file_cruft

def parseString(in_string, parse_point=parse_point):
    assert(isinstance(in_string, str))
    return parse_point.parseString(in_string)[:]

def parseFile(f, parse_point=parse_point):
    return parse_point.parseFile(f, parseAll=True)
