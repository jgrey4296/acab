""" Trie-based parser for constructing queries """
import logging as root_logger
import pyparsing as pp

from acab.abstract.parsing import util as PU
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing.consts import QUERY_SYMBOL, DOUBLEBAR, COLON, COMMA, COLON, DELIM, component_gap
from acab.abstract.parsing.consts import op, N, NG, QUERY_HEAD
from acab.working_memory.trie_wm.parsing import util as WMPU

from acab.working_memory.trie_wm.parsing.util import QUERY_FALLBACK_S, build_clause, build_query, build_assignment
from acab.working_memory.trie_wm.parsing.util import PARAM_CORE

from acab.config import AcabConfig

from .FactParser import PARAM_SEN, BASIC_SEN

logging = root_logger.getLogger(__name__)


assignment = PU.BIND + COLON + PARAM_SEN
assignmentList = pp.delimitedList(assignment, delim=COMMA)
fallback = DOUBLEBAR + assignmentList

# TODO add syntax for binding a sentence
# a.test.query..<$x?
# a.test.query..<$x(::Rule)?
# Core Query Chain
# TODO this can simplify down to a param sen + extra
clause = PARAM_SEN + QUERY_SYMBOL \
    + N(QUERY_FALLBACK_S, op(fallback))

clauses = pp.delimitedList(clause, delim=DELIM)

query_statement = Pfunc.STATEMENT_CONSTRUCTOR(QUERY_HEAD,
                                              BASIC_SEN,
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
