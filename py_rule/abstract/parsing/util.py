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


def construct_num(toks):
    # TODO: add in fractions and underscores
    if util.DECIMAL_S in toks[0]:
        return (util.FLOAT_S, float(toks[0].replace(util.DECIMAL_S,
                                                    '.')))
    else:
        return (util.INT_S, int(toks[0]))


def construct_sentence(toks):
    return Sentence(toks[:])


# UTILITIES
COMMENT   = pp.dblSlashComment

s         = pp.Suppress
op        = pp.Optional
emptyLine = s(pp.OneOrMore(pp.lineEnd))
opLn      = s(op(pp.lineEnd))
orm       = pp.OneOrMore
sLn       = s(pp.White(ws='\n', exact=1))


ARROW     = s(pp.Literal('->'))
COLON     = s(pp.Literal(':'))
COMMA     = s(pp.Or([pp.Literal(',') + opLn, pp.lineEnd]))
CPAR      = s(pp.Literal(')'))
DBLCOLON  = s(pp.Literal("::"))
DOLLAR    = s(pp.Literal('$'))
DOUBLEBAR = s(pp.Literal('||'))
end       = s(pp.Literal('end'))
HASH      = s(pp.Literal('#'))
OPAR      = s(pp.Literal('('))
QMARK     = s(pp.Literal('?'))
SLASH     = s(pp.Literal('/'))
SUB       = s(pp.Literal(util.SUB_S))
TILDE     = pp.Literal('~')
OBRACKET = s(pp.Literal('['))
CBRACKET = s(pp.Literal(']'))
LESS = s(pp.Literal('<'))
MORE = s(pp.Literal('>'))
VBAR = s(pp.Literal('|'))


NAME        = pp.Word(pp.alphas + util.UNDERSCORE_S)
NAME.setParseAction(lambda t: (util.NAME_S, t[0]))

NUM         = pp.Word(pp.nums + util.SUB_S + util.DECIMAL_S)
NUM.setParseAction(construct_num)

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(lambda t: (util.STRING_S, t[0].replace('"', '')))

REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: (util.REGEX_S, t[0][1:-1]))

BASIC_VALUE = pp.Or([NAME, NUM, STRING, REGEX])
BIND        = DOLLAR + NAME
