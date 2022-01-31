""" Trie-based parser for constructing queries """
import logging as root_logger

import pyparsing as pp
from acab.core.config.config import AcabConfig
import acab.core.data.default_structure as CDS
import acab.core.parsing.default_keys as PDS
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (COLLAPSE_CONTEXT, COLON, COMMA, DELIM, DOUBLEBAR, END,
                                          NG, QUERY, N, component_gap, op, zrm)
from acab.core.parsing.default_keys import OPERATOR, SEN, VALUE
from acab.core.parsing.annotation import ValueAnnotation
from acab.core.parsing.default_symbols import QUERY_HEAD
from acab.core.parsing.funcs import build_assignment
from acab.core.parsing.indented_block import IndentedBlock
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.constructors import build_query

from .FactParser import SENTENCE, op_sentence, annotations

logging = root_logger.getLogger(__name__)

# TODO add syntax for binding a sentence
# a.test.query..<$x?
# a.test.query..<$x(::Rule)?
#
# For Custom non-standard form queries
# eg: 1[a.b.$x]3?
# to capture a condition of how many $x's are permissible
HOTLOAD_QUERY_SEN = pp.Forward()
HOTLOAD_QUERY_OP  = pp.Forward()

assignment        = PU.BIND + COLON + SENTENCE
assignmentList    = PU.DELIMIST(assignment, delim=COMMA)
fallback          = DOUBLEBAR + assignmentList

# Build After comparison operators have been constructed:
op_path = HOTLOAD_QUERY_OP | op_sentence

basic_constraint = N(OPERATOR, op_path) + N(VALUE, zrm(SENTENCE))
basic_constraint.setParseAction(PConst.build_query_component)

COLLAPSE_CONTEXT      = COLLAPSE_CONTEXT.copy()
COLLAPSE_CONTEXT.setParseAction(lambda x: ValueRepeatAnnotation("constraint", CTX_OP.collect_var))

word_query_constraint = COLLAPSE_CONTEXT | basic_constraint

query_terminator = QUERY("QueryTerm")
query_terminator.addParseAction(lambda x: [ValueAnnotation(CDS.QUERY, True),
                                           ValueAnnotation(CDS.QUERY_FALLBACK, None)])

query_fallback   = op(fallback)
query_fallback.setParseAction(lambda x: ValueAnnotation(CDS.QUERY_FALLBACK, x[:]))

query_sen_end    = PU.PARAM_CORE(annotations,
                                 end=query_terminator + query_fallback)

clauses               = IndentedBlock(HOTLOAD_QUERY_SEN | SENTENCE)

query_statement       = PU.STATEMENT_CONSTRUCTOR(pp.Literal("::γ"),
                                                 clauses)

# Actions
clauses.setParseAction(build_query)
assignment.setParseAction(build_assignment)

# NAMING
# assignment.setName("FallbackAssignment")
# assignmentList.setName("FallbackAssignmentList")
# fallback.setName("QueryFallbackStatement")
query_sen_end.setName("QueryTerminator")
clauses.setName("Query")
query_statement.setName("QueryStatement")


# parse_point = clauses.ignore(PU.COMMENT)
parse_point = clauses

# Main parser:
def parseString(in_string, parse_point=parse_point):
    """ .a.b(>20)!d.$X, ... -> Query """
    return parse_point.parseString(in_string)[0]
