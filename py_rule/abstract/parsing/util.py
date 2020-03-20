"""
Pyparsing utilities
"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.sentence import Sentence
from py_rule import util

logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')


def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).setResultsName(name)

def N(name, parser):
    return parser.setResultsName(name)

def construct_sentence(toks):
    return Sentence(toks[:])


# UTILITIES
COMMENT   = pp.dblSlashComment

s         = pp.Suppress
op        = pp.Optional
emptyLine = s(pp.OneOrMore(pp.lineEnd))
opLn      = s(op(pp.lineEnd))
orm       = pp.OneOrMore
sLn       = s(pp.White(ws  ='\n', exact =1))

# Basic Syntax
ARROW     = s(pp.Literal('->'))
DBLARROW  = s(pp.Literal('=>'))
COLON     = s(pp.Literal(':'))
COMMA     = s(pp.Or([pp.Literal(',') + opLn, pp.lineEnd]))
DBLCOLON  = s(pp.Literal("::"))
DOLLAR    = s(pp.Literal('$'))
AT        = s(pp.Literal('@'))
DOUBLEBAR = s(pp.Literal('||'))
end       = s(pp.Literal('end'))
HASH      = s(pp.Literal("#"))
OPAR      = s(pp.Literal('('))
CPAR      = s(pp.Literal(')'))
QMARK     = s(pp.Literal('?'))
SLASH     = s(pp.Literal('/'))
DASH      = s(pp.Literal('-'))
# Careful: This isn't suppressed
TILDE     = pp.Literal('~')
OBRACKET  = s(pp.Literal('['))
CBRACKET  = s(pp.Literal(']'))
LESS      = s(pp.Literal('<'))
MORE      = s(pp.Literal('>'))
VBAR      = s(pp.Literal('|'))

MACRO_HEAD     = s(pp.Literal(util.MACRO_S))
STRUCT_HEAD  = s(pp.Literal(util.STRUCTURE_S))
FUNC_HEAD    = s(pp.Literal(util.FUNC_S))

# Basic Parsers
NAME        = pp.Word(util.WORD_COMPONENT_S)
NAME.setParseAction(lambda t: (util.NAME_S, t[0]))

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(lambda t: (util.STRING_S, t[0].replace('"', '')))

REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: (util.REGEX_S, t[0][1:-1]))

BASIC_VALUE = pp.Or([NAME, STRING, REGEX])
BIND        = DOLLAR + NAME
AT_BIND     = AT + NAME

OPERATOR_SUGAR = pp.Word(util.OPERATOR_SYNTAX_S)
