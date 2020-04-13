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

s          = pp.Suppress
op         = pp.Optional
orm        = pp.OneOrMore

COMMA         = s(pp.Literal(','))
emptyLine     = s(pp.lineEnd + pp.lineEnd)
opLn          = s(op(pp.lineEnd))
gap           = s(pp.OneOrMore(emptyLine))
component_gap = s(orm(pp.Or([pp.lineEnd, COMMA])))
file_cruft    = s(pp.ZeroOrMore(pp.Or([pp.lineEnd])))

# Basic Syntax
ARROW     = s(pp.Literal('->'))
DBLARROW  = s(pp.Literal('=>'))
COLON     = s(pp.Literal(':'))
DBLCOLON  = s(pp.Literal("::"))
DOLLAR    = s(pp.Literal('$'))
AT        = s(pp.Literal('@'))
DOUBLEBAR = s(pp.Literal('||'))
HASH      = s(pp.Literal("#"))
OPAR      = s(pp.Literal('('))
CPAR      = s(pp.Literal(')'))
QMARK     = s(pp.Literal('?'))
SLASH     = s(pp.Literal('/'))
DASH      = s(pp.Literal('-'))
TILDE     = s(pp.Literal('~'))
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
FACT_HEAD      = s(pp.Literal(util.FACT_HEAD_S))
STRUCT_HEAD    = s(pp.Literal(util.STRUCTURE_S))
FUNC_HEAD      = s(pp.Literal(util.FUNC_S))

VAR_SYMBOL     = s(pp.Literal(util.VAR_SYMBOL_S))
AT_BIND_SYMBOL = s(pp.Literal(util.AT_VAR_SYMBOL_S))

NEGATION_SYMBOL = N(util.NEGATION_S, pp.Literal(util.NEGATION_SYMBOL_S))
QUERY_SYMBOL    = s(pp.Literal(util.QUERY_SYMBOL_S))
TAG_SYMBOL      = s(pp.Literal(util.TAG_SYMBOL_S))
END             = s(pp.Literal(util.END_S))

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

arglist = VBAR + pp.delimitedList(BIND, delim=COMMA) + VBAR

tagName = TAG_SYMBOL + NAME
tagList    = op(N(util.TAG_S, pp.delimitedList(tagName, delim=DELIM) + emptyLine))


OPERATOR_SUGAR = pp.Word(util.OPERATOR_SYNTAX_S)

def construct_multi_sentences(toks):
    base_sen = toks[util.NAME_S][0]
    additional_sentences = toks[util.STATEMENT_S]

    new_sentences = []
    # combine
    for additional in additional_sentences:
        full_toks = base_sen.words[:] + additional.words[:]
        data = {}
        data.update(base_sen._data)
        data.update(additional._data)
        new_sen = Sentence(full_toks, data=data)
        new_sentences.append(new_sen)

    return new_sentences

def construct_sentence(toks):
    data = { util.NEGATION_S : False }
    if util.NEGATION_S in toks:
        data[util.NEGATION_S] = True
    return Sentence(toks[util.SEN_S][:], data=data)

def construct_statement(toks):
    # Take the statement, and add it to the location
    path  = toks[util.NAME_S][0]
    targs = []
    tags  = []
    if util.ARG_S in toks:
        # BIND's NAME returns a tuple of ('name', VARNAME)
        targs = [y for x,y in toks[util.ARG_S][:]]
    # Get Tags
    if util.TAG_S in toks:
        tags = [x[1] for x in toks[util.TAG_S]]

    obj_tuple  = toks[util.STATEMENT_S][0]
    obj_tuple[1].apply_onto(path, targs, tags=tags)
    obj_tuple[1].verify()
    return path

def STATEMENT_CONSTRUCTOR(head_p,
                          name_p,
                          body_p,
                          end=None,
                          args=True,
                          single_line=False,
                          parse_fn=None):
    """ Construct statements of the form:
    λ::a.location: |args| components end
    """
    line_p = emptyLine
    end_p  = END
    arg_p  = pp.empty

    if single_line:
        line_p = pp.empty
        end_p = pp.lineEnd
    elif end is not None:
        end_p = end

    if args:
        arg_p = op(NG(util.ARG_S, arglist + line_p))

    parser = head_p + DBLCOLON + NG(util.NAME_S, name_p) + COLON + op(pp.lineEnd) \
        + arg_p + tagList + NG(util.STATEMENT_S, body_p) + end_p

    if parse_fn is not None:
        parser.addParseAction(parse_fn)
    else:
        parser.addParseAction(construct_statement)
    return parser



# NAMING
file_cruft.setName("FileCruft")
gap.setName("RuleGap")

RULE_HEAD.setName("RuleHead")
QUERY_HEAD.setName("QueryHead")
TRANSFORM_HEAD.setName("TransformHead")
ACTION_HEAD.setName("ActionHead")
STRUCT_HEAD.setName("StructHead")
FUNC_HEAD.setName("FuncHead")
VAR_SYMBOL.setName("VarSymbol")
AT_BIND_SYMBOL.setName("AtSymbol")
NEGATION_SYMBOL.setName("NegationSymbol")
QUERY_SYMBOL.setName("QuerySymbol")
TAG_SYMBOL.setName("TagSymbol")
END.setName("End")

NAME.setName("NameWord")
STRING.setName("StringWord")
REGEX.setName("RegexWord")
BASIC_VALUE.setName("BasicValue")
BIND.setName("Binding")
AT_BIND.setName("AtBinding")
arglist.setName("StatementArgumentList")
tagName.setName("TagName")
tagList.setName("StatementTagList")
OPERATOR_SUGAR.setName("SyntaxSugar")
