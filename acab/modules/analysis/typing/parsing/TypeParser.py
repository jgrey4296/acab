"""
Pyparsing based DSL Fragment for types annotation
"""
import logging as logmod
import pyparsing as pp

from acab.core.parsing.consts import DOUBLEBAR, COLON, COMMA, DBLCOLON, DELIM, component_gap
from acab.core.parsing.consts import N, NG, op, OPAR, CPAR
from acab.core.value.sentence import Sentence
from acab.core.parsing import parsers as PU


from acab.core.config.config import AcabConfig

from . import util as TU

config = AcabConfig()
EXTENDED_LANGUAGE_SYNTAX_S = config.prepare("Parse.Patterns", "EXTENDED_LANGUAGE_SYNTAX")()

# BASIC SENTENCE NEEDS TO BE POPULATED
# eg: acab.modules.parsing.exlo.parsers.FactParser.BASIC_SEN
HOTLOAD_SEN  = pp.Forward()
TYPEDEC_CORE = pp.Forward()

EXTENDED_ATOM = pp.Word(EXTENDED_LANGUAGE_SYNTAX_S)
EXTENDED_ATOM.set_parse_action(lambda s, l, t: Sentence(t[0]))


VAR_OR_TYPE_DEC = PU.BIND | TYPEDEC_CORE

# a.type | α
TYPE_NAME       = HOTLOAD_SEN | EXTENDED_ATOM

# ::a.type($x, a.different.type)
TYPEDEC_CORE <<= (DBLCOLON + N(TU.SEN_S, TYPE_NAME)
                  + N(TU.ARG_S, op(OPAR
                                    + pp.delimited_list(VAR_OR_TYPE_DEC,
                                                        TU.PARAM_JOIN_S,
                                                        combine=False)
                                    + CPAR)))

TYPEDEC_CORE.set_parse_action(TU.make_type_dec)

# NAMING
TYPEDEC_CORE.set_name("TypeDeclaration")

parse_point = TYPEDEC_CORE

def parse_string(in_string):
    return parse_point.parse_string(in_string)[:]
