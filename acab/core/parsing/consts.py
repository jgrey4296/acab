# pylint: disable=bad-whitespace,invalid-name,line-too-long
#
import logging as logmod
import pyparsing as pp

from acab.core.config.config import AcabConfig
from acab.core.defaults import parse_keys as DS
from acab.core.defaults import parse_symbols as DSYM


logging = logmod.getLogger(__name__)

config = AcabConfig()

if config.prepare("Parse", "DEBUG_PARSERS", _type=bool)():
    import acab.core.parsing.debug_funcs as DBF
    DBF.debug_pyparsing()

COMMENT_RE       = config.prepare("Parse.Patterns", "COMMENT_RE", actions=[config.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])()
WORD_COMPONENT_S = config.prepare("Parse.Patterns", "WORD_COMPONENT")()
WHITE_SPACE      = config.prepare("Parse.Patterns", "WHITE_SPACE", actions=[config.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])()
TAB_S            = config.prepare("Parse.Patterns", "TAB", actions=[config.actions_e.STRIPQUOTE])()
pp.ParserElement.set_default_whitespace_chars(WHITE_SPACE)




s         = pp.Suppress
op        = pp.Optional
orm       = pp.OneOrMore
zrm       = pp.ZeroOrMore

s_lit = lambda x: s(pp.Literal(x))
s_key = lambda x: s(pp.Keyword(x))

COMMENT           = pp.Regex(COMMENT_RE)
END               = s_key(DSYM.END)
END.set_whitespace_chars(" \n")

ln                = s(pp.White("\n\r", max=1).set_whitespace_chars("\t "))
manyLine          = s(pp.White("\n\r", min=1).set_whitespace_chars("\t "))
emptyLine         = s(ln + manyLine)
opLn              = op(ln)
tab               = pp.White(TAB_S, min=2).set_whitespace_chars("\r\n")

emptyLine.set_name("emptyLine")
ln.set_name("line")
opLn.set_name("OptionalLine")
tab.set_name("tab")

def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).set_results_name(name)

def N(name, parser):
    return parser.set_results_name(name)

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
# DELIM     = ~END + (COMMA | (ln + tab))
DELIM    = (ln ^ COMMA).set_whitespace_chars(" \t")

COLLAPSE_CONTEXT = s_lit(DSYM.COLLAPSE_CONTEXT)

FUNC_SYMBOL      = s(pp.Word(DSYM.FUNC))

BIND             = s_lit(DSYM.BIND)
AT_BIND          = s_lit(DSYM.AT_BIND)

QUERY            = s_lit(DSYM.QUERY)
TAG              = s_lit(DSYM.TAG)

NEGATION         = pp.Literal(DSYM.NEGATION)(DS.NEGATION)

gap              = emptyLine #.set_fail_action(gap_fail_action)
component_gap    = s(orm(emptyLine))
file_cruft       = pp.ZeroOrMore(ln)

END.set_name("End")
file_cruft.set_name("EmptyLines")
gap.set_name("EmptyLine")
COLON.set_name("Colon")
DELIM.set_name("Delimiter")
