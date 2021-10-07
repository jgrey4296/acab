"""
Pyparsing based parser for types
"""
import logging as root_logger
import pyparsing as pp

from acab.core.parsing.consts import DOUBLEBAR, COLON, COMMA, DBLCOLON, DELIM, component_gap
from acab.core.parsing.consts import N, NG, op, OPAR, CPAR
from acab.core.data.values import Sentence
from acab.core.parsing import parsers as PU

from acab.modules.analysis.typing import util as TYU

from acab.core.config.config import AcabConfig

from . import util as TU

config = AcabConfig.Get()
EXTENDED_LANGUAGE_SYNTAX_S = config.prepare("Parse.Patterns", "EXTENDED_LANGUAGE_SYNTAX")()

# BASIC SENTENCE NEEDS TO BE POPULATED
# eg: acab.modules.parsing.exlo.parsers.FactParser.BASIC_SEN
HOTLOAD_SEN  = pp.Forward()
TYPEDEC_CORE = pp.Forward()

EXTENDED_ATOM = pp.Word(EXTENDED_LANGUAGE_SYNTAX_S)
EXTENDED_ATOM.setParseAction(lambda s, l, t: Sentence.build(t[0]))


VAR_OR_TYPE_DEC = pp.Or([PU.BIND, TYPEDEC_CORE])

# a.type | α
TYPE_NAME = pp.Or([HOTLOAD_SEN, EXTENDED_ATOM])

# ::a.type($x, a.different.type)
TYPEDEC_CORE <<= DBLCOLON + N(TYU.SEN_S, TYPE_NAME) \
    + N(TYU.ARG_S, op(OPAR
                      + pp.delimitedList(VAR_OR_TYPE_DEC,
                                         TYU.PARAM_JOIN_S,
                                         combine=False)
                      + CPAR))

TYPEDEC_CORE.setParseAction(TU.make_type_dec)

# NAMING
TYPEDEC_CORE.setName("TypeDeclarationStatement")

parse_point = TYPEDEC_CORE

def parseString(in_string):
    return parse_point.parseString(in_string)[:]
