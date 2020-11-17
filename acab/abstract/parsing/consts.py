#!/usr/bin/env python
# pylint: disable=bad-whitespace,invalid-name,line-too-long
#
import logging as root_logger
import pyparsing as pp

from acab.abstract.config.config import AcabConfig
from acab.abstract.config.modal import MODAL_DEFAULTS

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

COMMENT_RE       = util.value("Parse.Patterns", "COMMENT_RE", actions=[AcabConfig.actions_e.UNESCAPE])
WORD_COMPONENT_S = util.value("Parse.Patterns", "WORD_COMPONENT")
OPERATOR_SYNTAX  = util.value("Parse.Patterns", "OPERATOR_SYNTAX")
WHITE_SPACE      = util.value("Parse.Patterns", "WHITE_SPACE", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])
pp.ParserElement.setDefaultWhitespaceChars(WHITE_SPACE)

DEFAULT_NODE_DATA = {}
DEFAULT_NODE_DATA.update(MODAL_DEFAULTS)


ACTION_S         = util.value("Parse.Structure", "ACTION")
ANNOTATION_S     = util.value("Parse.Structure", "ANNOTATION")
ARG_S            = util.value("Parse.Structure", "PARAMS")
AT_BIND_S        = util.value("Value.Structure", "AT_BIND")
BIND_S           = util.value("Parse.Structure", "BIND")
CONSTRAINT_S     = util.value("Parse.Structure", "CONSTRAINT")
DEFAULT_ACTION_S = util.value("Parse.Structure", "DEFAULT_ACTION")
LEFT_S           = util.value("Parse.Structure", "LEFT")
MODAL_S          = util.value("Parse.Structure", "MODAL")
NAME_S           = util.value("Parse.Structure", "NAME")
NEGATION_S       = util.value("Parse.Structure", "NEGATION")
NODE_S           = util.value("Parse.Structure", "NODE")
OPERATOR_S       = util.value("Parse.Structure", "OPERATOR")
QUERY_FALLBACK_S = util.value("Parse.Structure", "QUERY_FALLBACK")
QUERY_S          = util.value("Parse.Structure", "QUERY")
RIGHT_S          = util.value("Parse.Structure", "RIGHT")
SEN_S            = util.value("Parse.Structure", "SEN")
STATEMENT_S      = util.value("Parse.Structure", "STATEMENT")
TAG_S            = util.value("Parse.Structure", "TAG")
TARGET_S         = util.value("Parse.Structure", "TARGET")
TRANSFORM_S      = util.value("Parse.Structure", "TRANSFORM")
TYPE_INSTANCE_S  = util.value("Parse.Structure", "TYPE_INSTANCE")
VALUE_S          = util.value("Parse.Structure", "VALUE")

# Primitives
ATOM_V   = util.value("Data", "TYPE_BOTTOM_NAME")
STRING_V = util.value("Type.Primitive", "STRING")
REGEX_V  = util.value("Type.Primitive", "REGEX")


s         = pp.Suppress
op        = pp.Optional
orm       = pp.OneOrMore
zrm       = pp.ZeroOrMore


emptyLine         = pp.Suppress(pp.lineEnd + pp.lineEnd)
END               = s(util.value("Symbols", "END", actions=[AcabConfig.actions_e.LITERAL]))
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
ARROW            = s(pp.Literal('->'))
DBLARROW         = s(pp.Literal('=>'))
COLON            = s(pp.Literal(':'))
DBLCOLON         = s(pp.Literal("::"))
DOLLAR           = s(pp.Literal('$'))
AT               = s(pp.Literal('@'))
DOUBLEBAR        = s(pp.Literal('||'))
HASH             = s(pp.Literal("#"))
OPAR             = s(pp.Literal('('))
CPAR             = s(pp.Literal(')'))
QMARK            = s(pp.Literal('?'))
SLASH            = s(pp.Literal('/'))
BSLASH           = s(pp.Literal('\\'))
DASH             = s(pp.Literal('-'))
TILDE            = s(pp.Literal('~'))
OBRACKET         = s(pp.Literal('['))
CBRACKET         = s(pp.Literal(']'))
LESS             = s(pp.Literal('<'))
MORE             = s(pp.Literal('>'))
DELIM            = pp.Or([COMMA, op(pp.lineEnd)])

RULE_HEAD        = s(util.value("Aliases", "RULE", actions=[AcabConfig.actions_e.KEYWORD]))
QUERY_HEAD       = s(util.value("Aliases", "QUERY", actions=[AcabConfig.actions_e.KEYWORD]))
TRANSFORM_HEAD   = s(util.value("Aliases", "TRANSFORM", actions=[AcabConfig.actions_e.KEYWORD]))
ACTION_HEAD      = s(util.value("Aliases", "ACTION", actions=[AcabConfig.actions_e.KEYWORD]))
FACT_HEAD        = s(util.value("Aliases", "FACT", actions=[AcabConfig.actions_e.KEYWORD]))
COLLAPSE_CONTEXT = s(util.value("Aliases", "CTX_COLLAPSE", actions=[AcabConfig.actions_e.LITERAL]))
AGENDA_HEAD      = s(util.value("Aliases", "AGENDA", actions=[AcabConfig.actions_e.KEYWORD]))
LAYER_HEAD       = s(util.value("Aliases", "LAYER", actions=[AcabConfig.actions_e.KEYWORD]))
PIPE_HEAD        = s(util.value("Aliases", "PIPE", actions=[AcabConfig.actions_e.KEYWORD]))

FUNC_SYMBOL      = s(pp.Word(util.value("Symbols", "FUNC")))

BIND_SYMBOL      = s(util.value("Symbols", "BIND", actions=[AcabConfig.actions_e.LITERAL]))
AT_BIND_SYMBOL   = s(util.value("Symbols", "AT_BIND", actions=[AcabConfig.actions_e.LITERAL]))

QUERY_SYMBOL     = s(util.value("Symbols", "QUERY", actions=[AcabConfig.actions_e.LITERAL]))
TAG_SYMBOL       = s(util.value("Symbols", "TAG", actions=[AcabConfig.actions_e.LITERAL]))

NEGATION_SYMBOL   = N(util.value("Parse.Structure", "NEGATION"),
                      util.value("Symbols", "NEGATION", actions=[AcabConfig.actions_e.LITERAL]))


END.setName("End")
file_cruft.setName("FileCruft")
gap.setName("RuleGap")
RULE_HEAD.setName("RuleHead")
QUERY_HEAD.setName("QueryHead")
TRANSFORM_HEAD.setName("TransformHead")
ACTION_HEAD.setName("ActionHead")
