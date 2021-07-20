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

COMMENT_RE       = config.prepare("Parse.Patterns", "COMMENT_RE", actions=[AcabConfig.actions_e.UNESCAPE])()
WORD_COMPONENT_S = config.prepare("Parse.Patterns", "WORD_COMPONENT")()
OPERATOR_SYNTAX  = config.prepare("Parse.Patterns", "OPERATOR_SYNTAX")()
WHITE_SPACE      = config.prepare("Parse.Patterns", "WHITE_SPACE", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])()
pp.ParserElement.setDefaultWhitespaceChars(WHITE_SPACE)

DEFAULT_NODE_DATA = {}
DEFAULT_NODE_DATA.update(config.defaults)


s         = pp.Suppress
op        = pp.Optional
orm       = pp.OneOrMore
zrm       = pp.ZeroOrMore

s_lit = lambda x: s(pp.Literal(x))
s_key = lambda x: s(pp.Keyword(x))

emptyLine         = s(pp.lineEnd + pp.lineEnd)
END               = s_key(DSYM.END)

COMMENT           = pp.Regex(COMMENT_RE)
opLn              = s(op(pp.lineEnd))

def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).setResultsName(name)

def N(name, parser):
    return parser.setResultsName(name)

def gap_fail_action(s, loc, expr, err):
    logging.warning("{}\n{}".format(str(err), err.markInputline()))


# Basic Syntax
ARROW     = s_lit('->')
AT        = s_lit('@')
BSLASH    = s_lit('\\')
CBRACKET  = s_lit(']')
COLON     = s_lit(':')
COMMA     = s_lit(',')
CPAR      = s_lit(')')
DASH      = s_lit('-')
DBLARROW  = s_lit('=>')
DBLCOLON  = s_lit("::")
DOLLAR    = s_lit('$')
DOUBLEBAR = s_lit('||')
HASH      = s_lit("#")
LESS      = s_lit('<')
MORE      = s_lit('>')
OBRACKET  = s_lit('[')
OPAR      = s_lit('(')
QMARK     = s_lit('?')
SLASH     = s_lit('/')
TILDE     = s_lit('~')
VBAR      = s_lit('|')
DELIM     = pp.Or([COMMA, op(pp.lineEnd)])

RULE_HEAD        = s_key(DSYM.RULE_HEAD)
QUERY_HEAD       = s_key(DSYM.QUERY_HEAD)
TRANSFORM_HEAD   = s_key(DSYM.TRANSFORM_HEAD)
ACTION_HEAD      = s_key(DSYM.ACTION_HEAD)
FACT_HEAD        = s_key(DSYM.FACT_HEAD)
AGENDA_HEAD      = s_key(DSYM.AGENDA_HEAD)
LAYER_HEAD       = s_key(DSYM.LAYER_HEAD)
PIPE_HEAD        = s_key(DSYM.PIPE_HEAD)

COLLAPSE_CONTEXT = s_lit(DSYM.COLLAPSE_CONTEXT)

FUNC_SYMBOL      = s(pp.Word(DSYM.FUNC))

BIND      = s_lit(DSYM.BIND)
AT_BIND   = s_lit(DSYM.AT_BIND)

QUERY     = s_lit(DSYM.QUERY)
TAG       = s_lit(DSYM.TAG)

NEGATION   = N(DS.NEGATION, pp.Literal(DSYM.NEGATION))

gap               = s(pp.OneOrMore(emptyLine)).setFailAction(gap_fail_action)
component_gap     = s(orm(pp.Or([pp.lineEnd, COMMA])))
file_cruft        = s(pp.ZeroOrMore(pp.Or([pp.lineEnd])))

END.setName("End")
file_cruft.setName("FileCruft")
gap.setName("RuleGap")
RULE_HEAD.setName("RuleHead")
QUERY_HEAD.setName("QueryHead")
TRANSFORM_HEAD.setName("TransformHead")
ACTION_HEAD.setName("ActionHead")
