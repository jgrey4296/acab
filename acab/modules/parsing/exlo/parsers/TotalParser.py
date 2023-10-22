""""
A Combined parser to parse rules and facts to assert
Handles files, and comments
"""
##-- imports
from __future__ import annotations
import pyparsing as pp
from acab.core.parsing.consts import (ARROW, COLON, COMMA, COMMENT, DELIM,
                                          DOUBLEBAR, NG, N, component_gap,
                                          file_cruft)
import acab.core.parsing.parsers as PU
from acab.core.parsing.funcs import strip_parse_type
from pyparsing import pyparsing_common as ppc

from . import ActionParser as AP
from . import FactParser as FP
from . import QueryParser as QP
from . import RuleParser as RP

##-- end imports

HOTLOAD_STATEMENTS = pp.Forward()
HOTLOAD_STATEMENTS.set_name("hl_statements")

file_component = HOTLOAD_STATEMENTS | FP.SEN_PLURAL

file_total     = pp.delimited_list(file_component, delim=component_gap)

# NAMING
# HOTLOAD_STATEMENTS.set_name("HotloadStatement")
# file_component.set_name("FileComponent")

parse_point = (file_cruft + file_total+ file_cruft).ignore(COMMENT)
# parse_point.set_name("Total Parser")
# parse_point = file_cruft +  file_total + file_cruft

def parse_string(in_string, parse_point=parse_point):
    assert(isinstance(in_string, str))
    return parse_point.parse_string(in_string)[:]

def parse_file(f, parse_point=parse_point):
    return parse_point.parse_file(f, parseAll=True)
