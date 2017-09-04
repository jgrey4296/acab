import logging as root_logger
import pyparsing as pp

from .FactParser import COMMA, VALBIND, PARAM_CORE, BIND, COLON, VALUE

from pyRule.utils import Bind,META_OP
from pyRule.Comparisons import COMP, Comparison, COMP_REVERSE_LOOKUP
from pyRule.Query import Query
from pyRule.Clause import Clause

pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

logging = root_logger.getLogger(__name__)

FALLBACK_IDEN = 'fallback'
MAIN_IDEN = 'main'
NOT_IDEN = 'NOT'

def buildClause(toks):
    #detect negation and annotate the clause with it
    if FALLBACK_IDEN in toks:
        fallback_toks = toks.fallback[:]
    else:
        fallback_toks = None
    if NOT_IDEN in toks:
        if fallback_toks is not None:
            raise Exception("Fallback bindings in negated clauses don't make sense")
        return Clause(toks.main[:], negated=True)
    else:
        return Clause(toks.main[:], fallback=fallback_toks)

def buildComparison(op, value):
    if isinstance(value, Bind):
        return Comparison(op, bind=value)
    else:
        return Comparison(op, value=value)

def buildQueryComponent(toks):
    node = toks[0]
    comparisons = toks[1:]
    node.set_meta_eval(META_OP.COMP, comparisons)
    return node


s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

OPAR = s(pp.Literal('('))
CPAR = s(pp.Literal(')'))
QMARK = pp.Literal('?')

LT = pp.Literal(COMP_REVERSE_LOOKUP[COMP.LT])
GT = pp.Literal(COMP_REVERSE_LOOKUP[COMP.GT])
NE = pp.Literal(COMP_REVERSE_LOOKUP[COMP.NE])
EQ = pp.Literal(COMP_REVERSE_LOOKUP[COMP.EQ])
REGMATCH = pp.Literal(COMP_REVERSE_LOOKUP[COMP.RE])
NOT = pp.Literal('~').setResultsName(NOT_IDEN)
SLASH = s(pp.Literal('/'))
REGEX = pp.Regex(r'/.+?/')
COMP_OP = pp.Or([LT, GT, NE, EQ, REGMATCH])
DOUBLEBAR = pp.Literal('||')

COMP_Internal = COMP_OP + pp.Or([VALBIND, REGEX])

comparison = OPAR + COMP_Internal \
             + op(pp.OneOrMore(COMMA + COMP_Internal))\
             + CPAR

assignment = BIND + COLON + VALUE
assignmentList = assignment + pp.ZeroOrMore(COMMA + assignment)
fallback = s(DOUBLEBAR) + assignmentList

#core component of a query, a modified param_fact_string
QueryCore = PARAM_CORE + op(comparison)

#Core Query Chain
clause = op(NOT) + (op(BIND) + pp.OneOrMore(QueryCore)).setResultsName(MAIN_IDEN) \
         + s(QMARK) + op(fallback).setResultsName(FALLBACK_IDEN)

clauses = clause + pp.ZeroOrMore(COMMA + clause)

#Actions

COMP_Internal.setParseAction(lambda toks: buildComparison(toks[0], toks[1]))
LT.setParseAction(lambda toks: COMP.LT)
GT.setParseAction(lambda toks: COMP.GT)
NE.setParseAction(lambda toks: COMP.NE)
EQ.setParseAction(lambda toks: COMP.EQ)
REGMATCH.setParseAction(lambda toks: COMP.RE)

REGEX.setParseAction(lambda toks: toks[0][1:-1])
QueryCore.setParseAction(buildQueryComponent)
#Clause, not negated:
clause.setParseAction(buildClause)
clauses.setParseAction(lambda toks: Query(toks[:]))

#assignment:
assignment.setParseAction(lambda toks: (toks[0], toks[1]))


#Main parser:
def parseString(s):
    """ .a.b(>20)!d.$X, ... -> Query """
    return clauses.parseString(s)[0]
