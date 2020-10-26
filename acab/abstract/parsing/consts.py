#!/usr/bin/env python
# pylint: disable=bad-whitespace,invalid-name,line-too-long
#
import logging as root_logger
import pyparsing as pp

from acab.config import AcabConfig

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

ARG_S             = util("Parsing.Structure", "ARG_S")
COMMENT_RE        = util("Visual.Symbols", "COMMENT_RE", AcabConfig.actions_e.UNESCAPE)
NAME_S            = util("Parsing.Structure", "NAME_S")
NEGATION_S        = util("Parsing.Structure", "NEGATION_S")
OPERATOR_SYNTAX_S = util("Visual.Symbols", "OPERATOR_SYNTAX_S")
SEN_S             = util("Parsing.Structure", "SEN_S")
STATEMENT_S       = util("Parsing.Structure", "STATEMENT_S")
TAG_S             = util("Parsing.Structure", "TAG_S")
WHITE_SPACE       = util("Visual.Symbols", "WHITE_SPACE", AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE)
WORD_COMPONENT_S  = util("Visual.Symbols", "WORD_COMPONENT_S")


pp.ParserElement.setDefaultWhitespaceChars(WHITE_SPACE)

s         = pp.Suppress
op        = pp.Optional
orm       = pp.OneOrMore
zrm       = pp.ZeroOrMore


emptyLine         = pp.Suppress(pp.lineEnd + pp.lineEnd)
END               = s(util("Visual.Symbols", "END_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
VBAR              = s(pp.Literal('|'))
COMMENT           = pp.Regex(COMMENT_RE)
COMMA             = s(pp.Literal(','))
opLn              = s(op(pp.lineEnd))

def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).setResultsName(name)

def N(name, parser):
    return parser.setResultsName(name)

def gap_fail_action(s, loc, expr, err):
    logging.warning("{}\n{}".format(str(err), err.markInputline()))


gap               = s(pp.OneOrMore(emptyLine)).setFailAction(gap_fail_action)
component_gap     = s(orm(pp.Or([pp.lineEnd, COMMA])))
file_cruft        = s(pp.ZeroOrMore(pp.Or([pp.lineEnd])))

# Basic Syntax
ARROW             = s(pp.Literal('->'))
DBLARROW          = s(pp.Literal('=>'))
COLON             = s(pp.Literal(':'))
DBLCOLON          = s(pp.Literal("::"))
DOLLAR            = s(pp.Literal('$'))
AT                = s(pp.Literal('@'))
DOUBLEBAR         = s(pp.Literal('||'))
HASH              = s(pp.Literal("#"))
OPAR              = s(pp.Literal('('))
CPAR              = s(pp.Literal(')'))
QMARK             = s(pp.Literal('?'))
SLASH             = s(pp.Literal('/'))
BSLASH            = s(pp.Literal('\\'))
DASH              = s(pp.Literal('-'))
TILDE             = s(pp.Literal('~'))
OBRACKET          = s(pp.Literal('['))
CBRACKET          = s(pp.Literal(']'))
LESS              = s(pp.Literal('<'))
MORE              = s(pp.Literal('>'))
DELIM             = pp.Or([COMMA, op(pp.lineEnd)])

RULE_HEAD         = s(util("Parsing.Statements", "RULE_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
QUERY_HEAD        = s(util("Parsing.Statements", "QUERY_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
TRANSFORM_HEAD    = s(util("Parsing.Statements", "TRANSFORM_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
ACTION_HEAD       = s(util("Parsing.Statements", "ACTION_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
FACT_HEAD         = s(util("Parsing.Statements", "FACT_HEAD_S", action=AcabConfig.actions_e.KEYWORD))

AGENDA_HEAD       = s(util("Parsing.Statements", "AGENDA_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
LAYER_HEAD        = s(util("Parsing.Statements", "LAYER_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
PIPE_HEAD         = s(util("Parsing.Statements", "PIPE_HEAD_S", action=AcabConfig.actions_e.KEYWORD))

FUNC_SYMBOL       = s(pp.Word(util("Visual.Symbols", "FUNC_SYMBOL_S")))

BIND_SYMBOL        = s(util("Visual.Symbols", "BIND_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
AT_BIND_SYMBOL    = s(util("Visual.Symbols", "AT_BIND_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))

QUERY_SYMBOL      = s(util("Visual.Symbols", "QUERY_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
TAG_SYMBOL        = s(util("Visual.Symbols", "TAG_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))
COLLAPSE_CONTEXT  = s(util("Visual.Symbols", "CTX_COLLAPSE_S", action=AcabConfig.actions_e.LITERAL))

NEGATION_SYMBOL   = N(util("Parsing.Structure", "NEGATION_S"),
                      util("Visual.Symbols", "NEGATION_SYMBOL_S", action=AcabConfig.actions_e.LITERAL))


END.setName("End")
file_cruft.setName("FileCruft")
gap.setName("RuleGap")

RULE_HEAD.setName("RuleHead")
QUERY_HEAD.setName("QueryHead")
TRANSFORM_HEAD.setName("TransformHead")
ACTION_HEAD.setName("ActionHead")
BIND_SYMBOL.setName("VarSymbol")
AT_BIND_SYMBOL.setName("AtSymbol")
NEGATION_SYMBOL.setName("NegationSymbol")
QUERY_SYMBOL.setName("QuerySymbol")
TAG_SYMBOL.setName("TagSymbol")

