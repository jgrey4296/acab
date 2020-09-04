""" Trie-based parser for constructing queries """
import logging as root_logger
import pyparsing as pp

from acab.abstract.parsing import util as PU
from acab.abstract.core.sentence import Sentence
from acab.abstract.rule.query import Query, QueryOp, QueryComponent
from acab.abstract.data.contexts import CTX_OP

from acab.working_memory.trie_wm import util as WMU

from acab.error.acab_parse_exception import AcabParseException
from acab.config import AcabConfig

from .FactParser import PARAM_CORE, PARAM_SEN, BASIC_SEN

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()
CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")
OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")
VALUE_S = util("Parsing.Structure", "VALUE_S")
QUERY_S = util("Parsing.Structure", "QUERY_S")
NEGATION_S = util("Parsing.Structure", "NEGATION_S")
FALLBACK_S = util("Parsing.Structure", "FALLBACK_S")
MAIN_CLAUSE_S = util("WorkingMemory.TrieWM", "MAIN_CLAUSE_S")

def build_constraint_list(toks):
    """ Build a constraint list """
    return (CONSTRAINT_S, [x[1] for x in toks[:]])

def build_query_component(toks):
    """ Build a comparison """
    op = toks[OPERATOR_S][0]
    return (CONSTRAINT_S, QueryComponent(op, param=toks[VALUE_S]))

def build_clause(toks):
    # detect negation and annotate the clause with it
    data = { QUERY_S : True,
             NEGATION_S : False,
             FALLBACK_S : None }
    if FALLBACK_S in toks:
        if NEGATION_S in toks:
            raise AcabParseException("Negated Fallback clauses don't make sense")
        data[FALLBACK_S] = toks[FALLBACK_S][:]
    if NEGATION_S in toks:
        data[NEGATION_S] = True

    return Sentence(toks[MAIN_CLAUSE_S][:], data=data)

def build_query(toks):
    query = Query(toks[:])
    return (query.type, query)

def build_assignment(toks):
    return (toks[0][1], toks[1])


# Build After comparison operators have been constructed:
HOTLOAD_QUERY_OP = pp.Forward()
HOTLOAD_QUERY_ANNOTATIONS = pp.Forward()

op_path = pp.Or([HOTLOAD_QUERY_OP, PU.OP_PATH_C(BASIC_SEN)])

QUERY_OP_Internal = PU.N(OPERATOR_S, op_path) \
    + PU.N(VALUE_S, PARAM_CORE(end=True))

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
# a.test.query..<$x?
# a.test.query..<$x(::Rule)?
# Core Query Chain
clause = PU.op(PU.NEGATION_SYMBOL) + PU.N(MAIN_CLAUSE_S, pp.ZeroOrMore(QueryCore)
                                          + QueryCore_end) \
                                          + PU.QUERY_SYMBOL\
                                          + PU.N(FALLBACK_S,
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
HOTLOAD_QUERY_ANNOTATIONS.setName("QueryAnnotation")
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
