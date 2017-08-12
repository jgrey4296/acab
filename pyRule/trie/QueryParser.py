import logging as root_logger
import pyparsing as pp
from .FactParser import OP,VALUE, COMMA, VALBIND, DOLLAR
from .Query import Query
from pyRule.utils import Bind, Comparison, QueryComponent, Clause, COMP 

logging = root_logger.getLogger(__name__)

def buildClause(toks):
    #detect negation and annotate the clause with it
    if 'NOT' in toks:
        return Clause(toks[1:], True)
    else:
        return Clause(toks, False)

def buildComparison(op, value):
    if isinstance(value, Bind):
        return Comparison(op,None,value)
    else:
        return Comparison(op, value, None)

def buildQueryComponent(toks):
    op = toks[0]
    if isinstance(toks[1], Bind):
        value = None
        bind = toks[1]
    else:
        value = toks[1]
        bind = None
    if len(toks) >= 3:
        comps = toks[2:]
    else:
        comps = []
    return QueryComponent(op, value, bind, comps)
        
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

OPAR = pp.Literal('(')
CPAR = pp.Literal(')')
OPAR = s(pp.Literal('('))
LT = pp.Literal('<')
GT = pp.Literal('>')
NE = pp.Literal('!=')
EQ = pp.Literal('==')

COMP_OP = pp.Or([LT, GT, NE, EQ])


COMP_Internal = COMP_OP + VALBIND

comparison = s(OPAR) + COMP_Internal \
       + op(pp.OneOrMore(s(COMMA) + COMP_Internal))\
       + s(CPAR)

#Core: OP (VALUE|BIND) (COMP)
core = OP + VALBIND + op(comparison)

#Core Query Chain
clause = pp.OneOrMore(core)

clauses = clause + pp.ZeroOrMore(s(COMMA) + clause)

#Actions
BIND.setParseAction(lambda toks: Bind(toks[1]))
COMP_Internal.setParseAction(lambda toks: buildComparison(toks[0], toks[1]))
LT.setParseAction(lambda toks: COMP.LT)
GT.setParseAction(lambda toks: COMP.GT)
NE.setParseAction(lambda toks: COMP.NE)
EQ.setParseAction(lambda toks: COMP.EQ)
core.setParseAction(lambda toks: buildQueryComponent(toks))
#Clause, not negated:
clause.setParseAction(lambda toks: Clause(toks[:], False))
                                
                                    
#Main parser:
def parseString(s):
    """ .a.b(>20)!d.$X, ... -> Query """
    data = clauses.parseString(s)[:]
    return Query(*data)
