""" Parser to query rule structure """
import logging as root_logger
import pyparsing as pp
from .FactParser import COMMA, VALBIND, BIND, COLON, VALUE
from .RuleParser import tagList
from .QueryParser import COMP_Internal, QMARK, NOT

pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

logging = root_logger.getLogger(__name__)

s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))
DOT = s(pp.Literal('.'))
OBRAK = s(pp.Literal('['))
CBRAK = s(pp.Literal(']'))

ident = pp.Or([pp.Literal(x) for x in "tags clause action bindings".split(" ")])

ruleTest = pp.Or([ tagList,
                   COMP_Internal
                  ])

# $x.tags[#something, #else]
# $x.bindings[$actor, $item, $utility]
# $x.clause[<4]
# $x.action

ruleQuery = op(NOT) + BIND + DOT + ident + OBRAK + ruleTest + CBRAK


def parseString(inString):
    assert(isinstance(inString, str))
    raise Exception("unimplemented")

