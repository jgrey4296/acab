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


# UTILITIES
COMMENT   = pp.dblSlashComment

s         = pp.Suppress
op        = pp.Optional
emptyLine = s(pp.lineEnd + pp.lineEnd)
opLn      = s(op(pp.lineEnd))
orm       = pp.OneOrMore
sLn       = s(pp.White(ws  ='\n', exact =1))

# Basic Syntax
ARROW     = s(pp.Literal('->'))
DBLARROW  = s(pp.Literal('=>'))
COLON     = s(pp.Literal(':'))
COMMA     = s(pp.Literal(',') + op(pp.lineEnd))
DBLCOLON  = s(pp.Literal("::"))
DOLLAR    = s(pp.Literal('$'))
AT        = s(pp.Literal('@'))
DOUBLEBAR = s(pp.Literal('||'))
END       = s(pp.lineStart + pp.Literal(util.END_S))
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
DELIM     = pp.Or([COMMA, op(pp.lineEnd)])


RULE_HEAD      = s(pp.Literal(util.RULE_HEAD_S))
QUERY_HEAD     = s(pp.Literal(util.QUERY_HEAD_S))
TRANSFORM_HEAD = s(pp.Literal(util.TRANSFORM_HEAD_S))
ACTION_HEAD    = s(pp.Literal(util.ACTION_HEAD_S))

STRUCT_HEAD    = s(pp.Literal(util.STRUCTURE_S))
FUNC_HEAD      = s(pp.Literal(util.FUNC_S))

VAR_SYMBOL     = s(pp.Literal(util.VAR_SYMBOL_S))
AT_BIND_SYMBOL = s(pp.Literal(util.AT_VAR_SYMBOL_S))

# Basic Parsers
NAME        = pp.Word(util.WORD_COMPONENT_S)
NAME.setParseAction(lambda t: (util.NAME_S, t[0]))

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(lambda t: (util.STRING_S, t[0].replace('"', '')))

REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: (util.REGEX_S, t[0][1:-1]))

BASIC_VALUE = pp.Or([NAME, STRING, REGEX])
BIND        = VAR_SYMBOL + NAME
AT_BIND     = AT_BIND_SYMBOL + NAME

# TODO set parse action
arglist = VBAR + pp.delimitedList(BIND, delim=COMMA) + VBAR

OPERATOR_SUGAR = pp.Word(util.OPERATOR_SYNTAX_S)

def construct_sentence(toks):
    return Sentence(toks[:])

def construct_statement(toks):
    # Take the statement, and add it to the location
    path = toks[util.NAME_S][0]
    # assert(path._data[util.BIND_S] is True), breakpoint()
    targs = []
    if util.ARG_S in toks:
        targs = toks[util.ARG_S][:]
    obj_tuple  = toks[util.STATEMENT_S][0]
    obj_tuple[1].set_name_and_vars(path, targs)

    return path

def STATEMENT_CONSTRUCTOR(head_p, name_p, body_p, end=None, args=True, single_line=False):
    """ Construct statements of the form:
    λ::a.location: |args| components end
    """
    line_p = emptyLine
    end_p  = END
    arg_p  = pp.empty

    if single_line:
        line_p = pp.empty

    if end is not None:
        end_p = end

    if args:
        arg_p = op(NG(util.ARG_S, arglist + line_p))

    parser = head_p + DBLCOLON + NG(util.NAME_S, name_p) + COLON + op(pp.lineEnd) \
        + arg_p + NG(util.STATEMENT_S, body_p) + end_p

    parser.setParseAction(construct_statement)
    return parser
