"""
Pyparsing utilities
"""
# pylint: disable=bad-whitespace
import re
import logging as root_logger
import pyparsing as pp

from acab.config import AcabConfig
from acab.abstract.parsing import funcs as PFunc
from acab.abstract.parsing import consts as PConst
from acab.abstract.parsing.consts import emptyLine, s, op, orm, zrm, N, NG
from acab.abstract.parsing.consts import gap, component_gap
from acab.abstract.parsing.funcs import Fwd_ArgList, Fwd_TagList, STATEMENT_CONSTRUCTOR

logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(PConst.WHITE_SPACE)

# Basic Parsers
OPERATOR_SUGAR = pp.Word(PConst.OPERATOR_SYNTAX_S)

ATOM        = pp.Word(PConst.WORD_COMPONENT_S)
ATOM.setParseAction(lambda t: ('ATOM', t[0]))

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(pp.removeQuotes)
STRING.addParseAction(lambda toks: ('STRING', toks[0]))

# TODO add re.RegexFlag 's to parser: g and i
REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: ('REGEX', re.compile(t[0][1:-1])))

BASIC_VALUE = pp.Or([ATOM, STRING, REGEX])
BIND        = PConst.VAR_SYMBOL + ATOM
AT_BIND     = PConst.AT_BIND_SYMBOL + ATOM

Fwd_ArgList <<= PConst.VBAR + pp.delimitedList(BIND, delim=PConst.COMMA) + PConst.VBAR

tagName = PConst.TAG_SYMBOL + ATOM

Fwd_TagList <<= op(N(PConst.TAG_S,
                     pp.delimitedList(tagName, delim=PConst.DELIM)
                     + emptyLine))

# NAMING
ATOM.setName("NameWord")
STRING.setName("StringWord")
REGEX.setName("RegexWord")
BASIC_VALUE.setName("BasicValue")
BIND.setName("Binding")
AT_BIND.setName("AtBinding")
tagName.setName("TagName")
Fwd_TagList.setName("StatementTagList")
Fwd_ArgList.setName("StatementArgumentList")
