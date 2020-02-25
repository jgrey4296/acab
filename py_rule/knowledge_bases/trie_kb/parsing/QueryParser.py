""" Trie-based parser for constructing queries """
import logging as root_logger
import pyparsing as pp
import py_rule.abstract.comparison as C
from py_rule.abstract.query import Query
from py_rule.abstract.sentence import Sentence
from py_rule.knowledge_bases.trie_kb import util as KBU
from py_rule.abstract.parsing import util as PU
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from .FactParser import PARAM_CORE, TYPE_ANNOTATION, PARAM_SEN

logging = root_logger.getLogger(__name__)


# Operator hotloading:
def build_operators():
    """ Hotload comparison operators after they have been initialised """
    if COMP_OP.expr is not None:
        logging.warning("Comparison Operators parser overwrite")
    OP_STRS = [x for x in C.CompOp.op_list.keys()]
    COMP_OP << pp.Or([pp.Literal(x) for x in OP_STRS])


def build_constraint_list(toks):
    """ Build a constraint list """
    return (KBU.CONSTRAINT_S, toks[:])


def build_comparison(toks):
    """ Build a comparison """
    return C.Comparison(toks[KBU.OPERATOR_S], value=toks[KBU.VALUE_S])


def build_clause(toks):
    # detect negation and annotate the clause with it
    if KBU.FALLBACK_S in toks:
        fallback_toks = toks[KBU.FALLBACK_S][:]
    else:
        fallback_toks = None
    if KBU.NOT_S in toks:
        if fallback_toks is not None:
            raise PyRuleParseException("Negated Fallback clauses in don't make sense")
        return Sentence(toks[KBU.MAIN_CLAUSE_S][:],
                        negated=True,
                        is_query=True)
    else:
        return Sentence(toks[KBU.MAIN_CLAUSE_S][:],
                        fallback=fallback_toks,
                        is_query=True)


# Build After comparison operators have been constructed:
COMP_OP = pp.Forward()

NOT = PU.N(KBU.NOT_S, PU.TILDE)

COMP_Internal = PU.N(KBU.OPERATOR_S, COMP_OP) \
    + PU.N(KBU.VALUE_S, PARAM_CORE(end=True))

# defined earlier to work with named copies
COMP_Internal.setParseAction(build_comparison)

comp_or_typedef = pp.Or([PU.N(KBU.COMP_S, COMP_Internal),
                         PU.N(KBU.TYPE_DEC_S, TYPE_ANNOTATION)])

constraints = comp_or_typedef + PU.op(pp.OneOrMore(PU.COMMA + comp_or_typedef))

assignment = PU.BIND + PU.COLON + PARAM_SEN
assignmentList = assignment + pp.ZeroOrMore(PU.COMMA + assignment)
fallback = PU.DOUBLEBAR + assignmentList

# core component of a query, a modified PARAM_SEN
QueryCore = PARAM_CORE(constraints)
QueryCore_end = PARAM_CORE(constraints, end=True)

# Core Query Chain
clause = PU.op(NOT) + PU.N(KBU.MAIN_CLAUSE_S, pp.ZeroOrMore(QueryCore)
                              + QueryCore_end) \
                              + PU.QMARK \
                              + PU.N(KBU.FALLBACK_S,
                                     PU.op(fallback))

clauses = clause + pp.ZeroOrMore(PU.COMMA + clause)

# Actions
constraints.setParseAction(build_constraint_list)
clause.setParseAction(build_clause)
clauses.setParseAction(lambda toks: Query(toks[:]))
assignment.setParseAction(lambda toks: (toks[0][1], toks[1]))


# Main parser:
def parseString(in_string):
    """ .a.b(>20)!d.$X, ... -> Query """
    return clauses.parseString(in_string)[0]
