""" Trie-based parser for constructing queries """
import logging as root_logger
import pyparsing as pp
import py_rule.abstract.comparison as C
from py_rule.abstract.query import Query
from py_rule.abstract.sentence import Sentence
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.abstract.parsing import util as PU
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from .FactParser import PARAM_CORE, HOTLOAD_ANNOTATIONS, PARAM_SEN, BASIC_SEN

logging = root_logger.getLogger(__name__)


# Operator hotloading:
def build_operators():
    """ Hotload comparison operators after they have been initialised """
    if COMP_OP.expr is not None:
        logging.debug("Comparison Operators parser overwrite")
    OP_STRS = [x for x in C.CompOp.op_list.keys()]
    COMP_OP << pp.Or([pp.Literal(x) for x in OP_STRS] + [PU.OPERATOR_SUGAR])


def build_constraint_list(toks):
    """ Build a constraint list """
    return (WMU.CONSTRAINT_S, toks[:])


def build_comparison(toks):
    """ Build a comparison """
    return C.Comparison(toks[WMU.OPERATOR_S], param=toks[WMU.VALUE_S])


def build_clause(toks):
    # detect negation and annotate the clause with it
    data = { WMU.QUERY_S : True,
             WMU.NEGATION_S : False,
             WMU.FALLBACK_S : None }
    if WMU.FALLBACK_S in toks:
        if WMU.NEGATION_S in toks:
            raise PyRuleParseException("Negated Fallback clauses don't make sense")
        data[WMU.FALLBACK_S] = toks[WMU.FALLBACK_S][:]
    if WMU.NEGATION_S in toks:
        data[WMU.NEGATION_S] = True

    return Sentence(toks[WMU.MAIN_CLAUSE_S][:], data=data)


def build_query(toks):
    query = Query(toks[:])
    return (query._type, query)


# Build After comparison operators have been constructed:
COMP_OP = pp.Forward()

COMP_Internal = PU.N(WMU.OPERATOR_S, COMP_OP) \
    + PU.N(WMU.VALUE_S, PARAM_CORE(end=True))

# defined earlier to work with named copies
COMP_Internal.setParseAction(build_comparison)

comp_or_annotation = pp.Or([COMP_Internal, HOTLOAD_ANNOTATIONS])

constraints = pp.delimitedList(comp_or_annotation, delim=PU.COMMA)

assignment = PU.BIND + PU.COLON + PARAM_SEN
assignmentList = pp.delimitedList(assignment, delim=PU.COMMA)
fallback = PU.DOUBLEBAR + assignmentList

# core component of a query, a modified PARAM_SEN
QueryCore = PARAM_CORE(constraints)
QueryCore_end = PARAM_CORE(constraints, end=True)

# TODO add syntax for binding a sentence
# TODO add syntax for binding all leaves
# Core Query Chain
clause = PU.op(PU.NEGATION_SYMBOL) + PU.N(WMU.MAIN_CLAUSE_S, pp.ZeroOrMore(QueryCore)
                                          + QueryCore_end) \
                                          + PU.QUERY_SYMBOL\
                                          + PU.N(WMU.FALLBACK_S,
                                                 PU.op(fallback))

clauses = pp.delimitedList(clause, delim=PU.DELIM)

query_statement = PU.STATEMENT_CONSTRUCTOR(PU.QUERY_HEAD, BASIC_SEN, clauses)

# Actions
constraints.setParseAction(build_constraint_list)
clause.setParseAction(build_clause)
clauses.setParseAction(build_query)
assignment.setParseAction(lambda toks: (toks[0][1], toks[1]))

# NAMING
COMP_OP.setName("ComparisonOperators")
COMP_Internal.setName("ComparisonStatement")
comp_or_annotation.setName("ComparisonOrAnnotation")
constraints.setName("ConstraintList")
assignment.setName("FallbackAssignment")
assignmentList.setName("FallbackAssignmentList")
fallback.setName("QueryFallbackStatement")
QueryCore.setName("QueryCoreStatement")
QueryCore_end.setName("QueryCoreEndStatement")
clause.setName("QueryClause")
clauses.setName("QueryClauseContainer")
query_statement.setName("QueryDefinition")

# parse_point = clauses.ignore(PU.COMMENT)
parse_point = clauses

# Main parser:
def parseString(in_string):
    """ .a.b(>20)!d.$X, ... -> Query """
    return parse_point.parseString(in_string)[0][1]
