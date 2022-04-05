""" Trie-based parser for constructing queries """
import logging as logmod

import pyparsing as pp
from acab.core.config.config import AcabConfig
import acab.core.data.default_structure as CDS
import acab.core.parsing.default_keys as PDS
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (COLLAPSE_CONTEXT, COLON, COMMA, DELIM, DOUBLEBAR, END,
                                          NG, QUERY, N, component_gap, op, zrm, orm, ln, s)
from acab.core.parsing.default_keys import OPERATOR, SEN, VALUE
from acab.core.parsing.annotation import ValueAnnotation
from acab.core.parsing.default_symbols import QUERY_HEAD
from acab.core.parsing.funcs import build_assignment
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.constructors import build_query


from acab.modules.parsing.exlo.util import QUERY_HEAD
from .FactParser import SENTENCE, op_sentence, annotations

logging = logmod.getLogger(__name__)

# TODO add syntax for binding a sentence
# a.test.query..<$x?
# a.test.query..<$x(::Rule)?
#
# For Custom non-standard form queries
# eg: 1[a.b.$x]3?
# to capture a condition of how many $x's are permissible
HOTLOAD_QUERY_SEN = pp.Forward()
HOTLOAD_QUERY_SEN.set_name("hl_query_sen")
HOTLOAD_QUERY_OP  = pp.Forward()
HOTLOAD_QUERY_OP.set_name("hl_query_op")

assignment        = PU.BIND + COLON + SENTENCE
assignmentList    = PU.DELIMIST(assignment, delim=COMMA)
fallback          = DOUBLEBAR + assignmentList

# Build After comparison operators have been constructed:
op_path = HOTLOAD_QUERY_OP | op_sentence

basic_constraint = N(OPERATOR, op_path) + zrm(SENTENCE)(VALUE)
basic_constraint.set_parse_action(PConst.build_query_component)

COLLAPSE_CONTEXT      = COLLAPSE_CONTEXT.copy()
COLLAPSE_CONTEXT.set_parse_action(lambda x: ValueRepeatAnnotation("constraint", CTX_OP.collect_var))

word_query_constraint = COLLAPSE_CONTEXT | basic_constraint

query_terminator = QUERY("QueryTerm")
query_terminator.add_parse_action(lambda x: [ValueAnnotation(CDS.QUERY, True),
                                             ValueAnnotation(CDS.QUERY_FALLBACK, None)])

query_fallback   = op(fallback)
query_fallback.set_parse_action(lambda x: ValueAnnotation(CDS.QUERY_FALLBACK, x[:]))

query_sen_post_annotation = query_terminator + query_fallback

clauses               = pp.IndentedBlock((HOTLOAD_QUERY_SEN | SENTENCE) + op(s(ln)))
# clauses.add_condition(lambda s, l, t: all([CDS.QUERY in x.data for x in t[:]]))

query_statement       = PU.STATEMENT_CONSTRUCTOR(QUERY_HEAD, clauses)

# Actions
clauses.set_parse_action(build_query)
assignment.set_parse_action(build_assignment)

# NAMING
# assignment.set_name("FallbackAssignment")
# assignmentList.set_name("FallbackAssignmentList")
# fallback.set_name("QueryFallbackStatement")
query_sen_post_annotation.set_name("QueryTerminator")
clauses.set_name("Query")
query_statement.set_name("QueryStatement")


# parse_point = clauses.ignore(PU.COMMENT)
parse_point = clauses

# Main parser:
def parse_string(in_string, parse_point=parse_point):
    """ .a.b(>20)!d.$X, ... -> Query """
    return parse_point.parse_string(in_string)[0]
