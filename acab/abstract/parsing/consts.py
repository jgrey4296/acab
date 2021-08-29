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

COMMENT_RE       = config.prepare("Parse.Patterns", "COMMENT_RE", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])()
WORD_COMPONENT_S = config.prepare("Parse.Patterns", "WORD_COMPONENT")()
WHITE_SPACE      = config.prepare("Parse.Patterns", "WHITE_SPACE", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])()
TAB_S            = config.prepare("Parse.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])()
pp.ParserElement.setDefaultWhitespaceChars(WHITE_SPACE)
if config.prepare("Parse", "DEBUG_PARSERS")():
    logging.info("Enabling Debug on Named Parsers")
    pp.__diag__.enable_debug_on_named_expressions = True
    pp._defaultExceptionDebugAction = lambda i,l,e,c: print(f"Bad {e}: {c}")


DEFAULT_NODE_DATA = {}
DEFAULT_NODE_DATA.update(config.defaults)


s         = pp.Suppress
op        = pp.Optional
orm       = pp.OneOrMore
zrm       = pp.ZeroOrMore

s_lit = lambda x: s(pp.Literal(x))
s_key = lambda x: s(pp.Keyword(x))

COMMENT           = pp.Regex(COMMENT_RE)
END               = s_key(DSYM.END)

emptyLine         = s(pp.White("\r\n", min=2))("emptyline")
ln                = s(pp.White("\n", max=1))("newline")
opLn              = op(ln)("optional newline")
tab               = pp.White(TAB_S, min=2)

emptyLine.setName("emptyLine")
ln.setName("line")
opLn.setName("OptionalLine")
tab.setName("tab")

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
COLON.setName("Colon")
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
DELIM     = COMMA | (ln + tab)
DELIM.setName("Delimiter")

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

gap               = emptyLine #.setFailAction(gap_fail_action)
component_gap     = s(orm(emptyLine))
file_cruft        = pp.ZeroOrMore(ln)

END.setName("End")
file_cruft.setName("EmptyLines")
gap.setName("EmptyLine")
# RULE_HEAD.setName("RuleHead")
# QUERY_HEAD.setName("QueryHead")
# TRANSFORM_HEAD.setName("TransformHead")
# ACTION_HEAD.setName("ActionHead")
