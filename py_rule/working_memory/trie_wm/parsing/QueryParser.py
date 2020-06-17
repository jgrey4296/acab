""" Trie-based parser for constructing queries """
import logging as root_logger
import pyparsing as pp

from acab.abstract.query import Query, QueryOp, QueryComponent
from acab.abstract.sentence import Sentence
from acab.abstract.contexts import CTX_OP

from acab.working_memory.trie_wm import util as WMU
from acab.abstract.parsing import util as PU

from acab.error.acab_parse_exception import AcabParseException

from .FactParser import PARAM_CORE, PARAM_SEN, BASIC_SEN

logging = root_logger.getLogger(__name__)

def build_constraint_list(toks):
    """ Build a constraint list """
    return (WMU.CONSTRAINT_S, [x[1] for x in toks[:]])

def build_query_component(toks):
    """ Build a comparison """
    return (WMU.CONSTRAINT_S, QueryComponent(toks[WMU.OPERATOR_S], param=toks[WMU.VALUE_S]))

def build_clause(toks):
    # detect negation and annotate the clause with it
    data = { WMU.QUERY_S : True,
             WMU.NEGATION_S : False,
             WMU.FALLBACK_S : None }
    if WMU.FALLBACK_S in toks:
        if WMU.NEGATION_S in toks:
            raise AcabParseException("Negated Fallback clauses don't make sense")
        data[WMU.FALLBACK_S] = toks[WMU.FALLBACK_S][:]
    if WMU.NEGATION_S in toks:
        data[WMU.NEGATION_S] = True

    return Sentence(toks[WMU.MAIN_CLAUSE_S][:], data=data)

def build_query(toks):
    query = Query(toks[:])
    return (query.type, query)

def build_assignment(toks):
    return (toks[0][1], toks[1])


# Build After comparison operators have been constructed:
HOTLOAD_QUERY_OP = pp.Forward()
HOTLOAD_QUERY_ANNOTATIONS = pp.Forward()

# TODO: add \ but not for sugar syntax
QUERY_OP_Internal = PU.N(WMU.OPERATOR_S, HOTLOAD_QUERY_OP) \
    + PU.N(WMU.VALUE_S, PARAM_CORE(end=True))

# defined earlier to work with named copies
QUERY_OP_Internal.setParseAction(build_query_component)

COLLAPSE_CONTEXT = PU.COLLAPSE_CONTEXT
COLLAPSE_CONTEXT.setParseAction(lambda x: (None, CTX_OP.collapse))

query_or_annotation = pp.Or([QUERY_OP_Internal, COLLAPSE_CONTEXT, HOTLOAD_QUERY_ANNOTATIONS])

constraints = pp.delimitedList(query_or_annotation, delim=PU.COMMA)

assignment = PU.BIND + PU.COLON + PARAM_SEN
assignmentList = pp.delimitedList(assignment, delim=PU.COMMA)
fallback = PU.DOUBLEBAR + assignmentList

# core component of a query, a modified PARAM_SEN
QueryCore = PARAM_CORE(constraints)
QueryCore_end = PARAM_CORE(constraints, end=True)

# TODO add syntax for binding a sentence
# TODO add syntax for binding all leaves
# a.test.query..<$x?
# a.test.query..<$x(::Rule)?
# Core Query Chain
clause = PU.op(PU.NEGATION_SYMBOL) + PU.N(WMU.MAIN_CLAUSE_S, pp.ZeroOrMore(QueryCore)
                                          + QueryCore_end) \
                                          + PU.QUERY_SYMBOL\
                                          + PU.N(WMU.FALLBACK_S,
                                                 PU.op(fallback))

clauses = pp.delimitedList(clause, delim=PU.DELIM)

query_statement = PU.STATEMENT_CONSTRUCTOR(PU.QUERY_HEAD,
                                           BASIC_SEN,
                                           clauses + PU.component_gap)

# Actions
constraints.setParseAction(build_constraint_list)
clause.setParseAction(build_clause)
clauses.setParseAction(build_query)
assignment.setParseAction(build_assignment)

# NAMING
HOTLOAD_QUERY_OP.setName("QueryOperators")
QUERY_OP_Internal.setName("Query_Statements")
query_or_annotation.setName("QueryOrAnnotation")
constraints.setName("ConstraintList")
assignment.setName("FallbackAssignment")
assignmentList.setName("FallbackAssignmentList")
fallback.setName("QueryFallbackStatement")
QueryCore.setName("QueryCoreStatement")
QueryCore_end.setName("QueryCoreEndStatement")
clause.setName("QueryComponent")
clauses.setName("QueryComponentContainer")
query_statement.setName("QueryDefinition")

# parse_point = clauses.ignore(PU.COMMENT)
parse_point = clauses

# Main parser:
def parseString(in_string):
    """ .a.b(>20)!d.$X, ... -> Query """
    return parse_point.parseString(in_string)[0][1]
