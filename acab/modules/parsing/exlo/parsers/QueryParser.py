""" Trie-based parser for constructing queries """
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.default_structure import QUERY_FALLBACK
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import (COLON, COMMA, DELIM, DOUBLEBAR, NG,
                                          N, component_gap, op)
from acab.abstract.parsing.default_symbols import QUERY_HEAD
from acab.abstract.parsing.consts import QUERY
from acab.abstract.parsing.funcs import build_assignment, build_clause
from acab.modules.parsing.exlo.constructors import build_query

from .FactParser import BASIC_SEN, PARAM_SEN

logging = root_logger.getLogger(__name__)


assignment = PU.BIND + COLON + PARAM_SEN
assignmentList = pp.delimitedList(assignment, delim=COMMA)
fallback = DOUBLEBAR + assignmentList

# TODO add syntax for binding a sentence
# a.test.query..<$x?
# a.test.query..<$x(::Rule)?
# Core Query Chain
# TODO this can simplify down to a param sen + extra
clause = PARAM_SEN + QUERY \
    + N(QUERY_FALLBACK, op(fallback))

clauses = pp.delimitedList(clause, delim=DELIM)

query_statement = PU.STATEMENT_CONSTRUCTOR(BASIC_SEN,
                                           clauses + component_gap)

# Actions
clause.setParseAction(build_clause)
clauses.setParseAction(build_query)
assignment.setParseAction(build_assignment)

# NAMING
assignment.setName("FallbackAssignment")
assignmentList.setName("FallbackAssignmentList")
fallback.setName("QueryFallbackStatement")
clause.setName("QueryComponent")
clauses.setName("QueryComponentContainer")
query_statement.setName("QueryDefinition")


# parse_point = clauses.ignore(PU.COMMENT)
parse_point = clauses

# Main parser:
def parseString(in_string, parse_point=parse_point):
    """ .a.b(>20)!d.$X, ... -> Query """
    return parse_point.parseString(in_string)[0][1]
