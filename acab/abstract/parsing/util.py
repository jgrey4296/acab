"""
Pyparsing utilities
"""
import re
import logging as root_logger
import pyparsing as pp

from acab.abstract.sentence import Sentence
from acab.abstract import type_base as TB
from acab.config import AcabConfig

util = AcabConfig.Get()

TAG_S = util("Parsing.Structure", "TAG_S")

OPERATOR_SYNTAX_S = util("Parsing", "OPERATOR_SYNTAX_S")
WORD_COMPONENT_S = util("Parsing", "WORD_COMPONENT_S")
WHITE_SPACE = util("Parsing", "WHITE_SPACE", AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE)
COMMENT_RE = util("Parsing", "COMMENT_RE", AcabConfig.actions_e.UNESCAPE)

NAME_S = util("Parsing.Structure", "NAME_S")
STATEMENT_S = util("Parsing.Structure", "STATEMENT_S")
NEGATION_S = util("Parsing.Structure", "NEGATION_S")
SEN_S = util("Parsing.Structure", "SEN_S")
ARG_S = util("Parsing.Structure", "ARG_S")
TAG_S = util("Parsing.Structure", "TAG_S")

logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(WHITE_SPACE)


def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).setResultsName(name)

def N(name, parser):
    return parser.setResultsName(name)


# UTILITIES
COMMENT   = pp.Regex(COMMENT_RE)

s          = pp.Suppress
op         = pp.Optional
orm        = pp.OneOrMore
zrm        = pp.ZeroOrMore

COMMA         = s(pp.Literal(','))
emptyLine     = s(pp.lineEnd + pp.lineEnd)
opLn          = s(op(pp.lineEnd))
gap           = s(pp.OneOrMore(emptyLine)).setFailAction(lambda s,
                                                         loc,
                                                         expr,
                                                         err: print("{}\n{}".format(str(err), err.markInputline())))
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
BSLASH    = s(pp.Literal('\\'))
DASH      = s(pp.Literal('-'))
TILDE     = s(pp.Literal('~'))
OBRACKET  = s(pp.Literal('['))
CBRACKET  = s(pp.Literal(']'))
LESS      = s(pp.Literal('<'))
MORE      = s(pp.Literal('>'))
VBAR      = s(pp.Literal('|'))
DELIM     = pp.Or([COMMA, op(pp.lineEnd)])

RULE_HEAD      = s(util("Parsing.Statements", "RULE_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
QUERY_HEAD     = s(util("Parsing.Statements", "QUERY_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
TRANSFORM_HEAD = s(util("Parsing.Statements", "TRANSFORM_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
ACTION_HEAD    = s(util("Parsing.Statements", "ACTION_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
FACT_HEAD      = s(util("Parsing.Statements", "FACT_HEAD_S", action=AcabConfig.actions_e.KEYWORD))

AGENDA_HEAD    = s(util("Parsing.Statements", "AGENDA_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
LAYER_HEAD     = s(util("Parsing.Statements", "LAYER_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
PIPE_HEAD      = s(util("Parsing.Statements", "PIPE_HEAD_S", action=AcabConfig.actions_e.KEYWORD))

FUNC_SYMBOL    = s(pp.Word(util("Parsing", "FUNC_SYMBOL_S")))

VAR_SYMBOL     = s(util("Parsing", "VAR_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
AT_BIND_SYMBOL = s(util("Parsing", "AT_VAR_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))

NEGATION_SYMBOL = N(util("Parsing.Structure", "NEGATION_S"),
                    util("Parsing", "NEGATION_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
QUERY_SYMBOL    = s(util("Parsing", "QUERY_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
TAG_SYMBOL      = s(util("Parsing", "TAG_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
END             = s(util("Parsing", "END_S", action=AcabConfig.actions_e.LITERAL))
COLLAPSE_CONTEXT = s(util("Parsing", "CTX_COLLAPSE_S", action=AcabConfig.actions_e.LITERAL))

# Basic Parsers
OPERATOR_SUGAR = pp.Word(OPERATOR_SYNTAX_S)

ATOM        = pp.Word(WORD_COMPONENT_S)
ATOM.setParseAction(lambda t: (TB.ATOM, t[0]))

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(pp.removeQuotes)
STRING.addParseAction(lambda toks: (TB.STRING, toks[0]))

# TODO add re.RegexFlag 's to parser: g and i
# TODO compile the regex
REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: (TB.REGEX, re.compile(t[0][1:-1])))

BASIC_VALUE = pp.Or([ATOM, STRING, REGEX])
BIND        = VAR_SYMBOL + ATOM
AT_BIND     = AT_BIND_SYMBOL + ATOM

arglist = VBAR + pp.delimitedList(BIND, delim=COMMA) + VBAR

tagName = TAG_SYMBOL + ATOM
tagList    = op(N(TAG_S, pp.delimitedList(tagName, delim=DELIM) + emptyLine))



def construct_multi_sentences(toks):
    base_sen = toks[NAME_S][0]
    additional_sentences = toks[STATEMENT_S]

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
    data = { NEGATION_S : False }
    if NEGATION_S in toks:
        data[NEGATION_S] = True
    return Sentence(toks[SEN_S][:], data=data)

def construct_statement(toks):
    # Take the statement, and add it to the location
    sen  = toks[NAME_S][0]
    targs = []
    tags  = []
    if ARG_S in toks:
        # BIND's ATOM returns a tuple of ('name', VARNAME)
        targs = [y for x,y in toks[ARG_S][:]]
    # Get Tags
    if TAG_S in toks:
        tags = [x[1] for x in toks[TAG_S]]

    obj_tuple  = toks[STATEMENT_S][0]
    obj_tuple[1].apply_vars(targs).apply_tags(tags)

    new_sentence = sen.attach_statement(obj_tuple[1]).verify()

    return new_sentence

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
        arg_p = op(NG(ARG_S, arglist + line_p))

    head_hint = OPAR + DBLCOLON + head_p + CPAR

    parser = NG(NAME_S, name_p) + COLON + s(head_hint) + op(pp.lineEnd) \
        + arg_p + tagList + NG(STATEMENT_S, body_p) + end_p

    if parse_fn is not None:
        parser.addParseAction(parse_fn)
    else:
        parser.addParseAction(construct_statement)
    return parser


def OP_PATH_C(sen):
    op_path = FUNC_SYMBOL + sen
    op_path.setName("Operator_Path")
    return op_path

# NAMING
file_cruft.setName("FileCruft")
gap.setName("RuleGap")

RULE_HEAD.setName("RuleHead")
QUERY_HEAD.setName("QueryHead")
TRANSFORM_HEAD.setName("TransformHead")
ACTION_HEAD.setName("ActionHead")
VAR_SYMBOL.setName("VarSymbol")
AT_BIND_SYMBOL.setName("AtSymbol")
NEGATION_SYMBOL.setName("NegationSymbol")
QUERY_SYMBOL.setName("QuerySymbol")
TAG_SYMBOL.setName("TagSymbol")
END.setName("End")

ATOM.setName("NameWord")
STRING.setName("StringWord")
REGEX.setName("RegexWord")
BASIC_VALUE.setName("BasicValue")
BIND.setName("Binding")
AT_BIND.setName("AtBinding")
arglist.setName("StatementArgumentList")
tagName.setName("TagName")
tagList.setName("StatementTagList")
OPERATOR_SUGAR.setName("SyntaxSugar")
