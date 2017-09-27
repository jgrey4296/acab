""" Trie-based parser for constructing queries """
import logging as root_logger
import pyparsing as pp
from pyRule.utils import Bind, META_OP
from pyRule.Comparisons import COMP, Comparison, COMP_REVERSE_LOOKUP
from pyRule.Query import Query
from pyRule.Clause import Clause
from .FactParser import COMMA, VALBIND, PARAM_CORE, BIND, COLON, VALUE
from . import RuleQueryParser as RQP
import IPython
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

logging = root_logger.getLogger(__name__)

FALLBACK_IDEN = 'fallback'
MAIN_IDEN = 'main'
NOT_IDEN = 'NOT'
COMP_IDEN = 'comparison'
RULEBIND_IDEN = 'rulebind'

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

def buildComparison(operator, value):
    if isinstance(value, Bind):
        return Comparison(operator, bind=value)
    else:
        return Comparison(operator, value=value)

def buildQueryComponent(toks):
    node = toks[0]
    if COMP_IDEN in toks:
        comparisons = toks[COMP_IDEN][:]
        node.set_meta_eval(META_OP.COMP, comparisons)
    elif RULEBIND_IDEN in toks:
        rulebind = toks[RULEBIND_IDEN][0]
        node.set_meta_eval(META_OP.RULEBIND, rulebind)
    return node


s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

OPAR = s(pp.Literal('('))
CPAR = s(pp.Literal(')'))
QMARK = s(pp.Literal('?'))

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
META = s(pp.Literal('^'))

COMP_Internal = COMP_OP + pp.Or([VALBIND, REGEX])

#defined earlier to work with named copies
LT.setParseAction(lambda toks: COMP.LT)
GT.setParseAction(lambda toks: COMP.GT)
NE.setParseAction(lambda toks: COMP.NE)
EQ.setParseAction(lambda toks: COMP.EQ)
REGMATCH.setParseAction(lambda toks: COMP.RE)
COMP_Internal.setParseAction(lambda toks: buildComparison(toks[0], toks[1]))
REGEX.setParseAction(lambda toks: toks[0][1:-1])


#TODO: add comparison features for testing rule components
#ie: has tag, has penumbral conditions,
#has penumbral actions
ruleBind = OPAR + META + BIND + CPAR
ruleBind = ruleBind(RULEBIND_IDEN)

comparison = OPAR + COMP_Internal \
             + op(pp.OneOrMore(COMMA + COMP_Internal))\
             + CPAR
comparison = comparison(COMP_IDEN)

assignment = BIND + COLON + VALUE
assignmentList = assignment + pp.ZeroOrMore(COMMA + assignment)
fallback = s(DOUBLEBAR) + assignmentList

#core component of a query, a modified param_fact_string
QueryCore = PARAM_CORE + op(pp.Or([comparison,
                                   ruleBind]))

#Core Query Chain
clause = op(NOT) + (op(BIND) + pp.OneOrMore(QueryCore)).setResultsName(MAIN_IDEN) \
         + QMARK + op(fallback).setResultsName(FALLBACK_IDEN)

clauses = clause + pp.ZeroOrMore(COMMA + clause)

#Actions
QueryCore.setParseAction(buildQueryComponent)
clause.setParseAction(buildClause)
clauses.setParseAction(lambda toks: Query(toks[:]))

#assignment:
assignment.setParseAction(lambda toks: (toks[0], toks[1]))


#Main parser:
def parseString(in_string):
    """ .a.b(>20)!d.$X, ... -> Query """
    return clauses.parseString(in_string)[0]
