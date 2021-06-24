# pylint: disable=bad-whitespace,invalid-name,line-too-long
#
import logging as root_logger
import pyparsing as pp

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence
import acab.abstract.parsing.default_structure as DS
import acab.abstract.parsing.default_symbols as DSYM

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

COMMENT_RE       = config.value("Parse.Patterns", "COMMENT_RE", actions=[AcabConfig.actions_e.UNESCAPE])
WORD_COMPONENT_S = config.value("Parse.Patterns", "WORD_COMPONENT")
OPERATOR_SYNTAX  = config.value("Parse.Patterns", "OPERATOR_SYNTAX")
WHITE_SPACE      = config.value("Parse.Patterns", "WHITE_SPACE", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])
pp.ParserElement.setDefaultWhitespaceChars(WHITE_SPACE)

DEFAULT_NODE_DATA = {}
DEFAULT_NODE_DATA.update(config.defaults)


s         = pp.Suppress
op        = pp.Optional
orm       = pp.OneOrMore
zrm       = pp.ZeroOrMore


emptyLine         = pp.Suppress(pp.lineEnd + pp.lineEnd)
END               = s(DSYM.END)
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

RULE_HEAD        = s(DSYM.RULE_HEAD)
QUERY_HEAD       = s(DSYM.QUERY_HEAD)
TRANSFORM_HEAD   = s(DSYM.TRANSFORM_HEAD)
ACTION_HEAD      = s(DSYM.ACTION_HEAD)
FACT_HEAD        = s(DSYM.FACT_HEAD)
COLLAPSE_CONTEXT = s(DSYM.COLLAPSE_CONTEXT)
AGENDA_HEAD      = s(DSYM.AGENDA_HEAD)
LAYER_HEAD       = s(DSYM.LAYER_HEAD)
PIPE_HEAD        = s(DSYM.PIPE_HEAD)

FUNC_SYMBOL      = s(pp.Word(DSYM.FUNC))

BIND      = s(DSYM.BIND)
AT_BIND   = s(DSYM.AT_BIND)

QUERY     = s(DSYM.QUERY)
TAG       = s(DSYM.TAG)

NEGATION   = N(config.value("Parse.Structure", "NEGATION"),
               DSYM.NEGATION)


END.setName("End")
file_cruft.setName("FileCruft")
gap.setName("RuleGap")
RULE_HEAD.setName("RuleHead")
QUERY_HEAD.setName("QueryHead")
TRANSFORM_HEAD.setName("TransformHead")
ACTION_HEAD.setName("ActionHead")
