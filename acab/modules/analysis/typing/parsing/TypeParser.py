"""
Pyparsing based parser for types
"""
import logging as root_logger
import pyparsing as pp

from acab.abstract.parsing.consts import DOUBLEBAR, COLON, COMMA, DBLCOLON, DELIM, component_gap
from acab.abstract.parsing.consts import N, NG, op, OPAR, CPAR
from acab.abstract.core.values import Sentence
from acab.abstract.parsing import parsers as PU

from acab.modules.analysis.typing import util as TYU

from acab.abstract.config.config import AcabConfig

config = AcabConfig.Get()
TYPE_INSTANCE_S            = config.value("Value.Structure", "TYPE_INSTANCE")
EXTENDED_LANGUAGE_SYNTAX_S = config.value("Parse.Patterns", "EXTENDED_LANGUAGE_SYNTAX")

def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[TYU.SEN_S]
    args = []
    if TYU.ARG_S in toks:
        args = [x[1] if isinstance(x, tuple) else x for x in toks[TYU.ARG_S][:]]
    return (TYPE_INSTANCE_S, Sentence.build(path, params=args))

# BASIC SENTENCE NEEDS TO BE POPULATED
# eg: acab.working_memory.trie_wm.parsing.FactParser.basic_fact_string
HOTLOAD_BASIC_SEN= pp.Forward()
TYPEDEC_CORE = pp.Forward()

EXTENDED_ATOM = pp.Word(EXTENDED_LANGUAGE_SYNTAX_S)
EXTENDED_ATOM.setParseAction(lambda s, l, t: Sentence.build(t[0]))


VAR_OR_TYPE_DEC = pp.Or([PU.BIND, TYPEDEC_CORE])

TYPE_NAME = pp.Or([HOTLOAD_BASIC_SEN, EXTENDED_ATOM])

TYPEDEC_CORE <<= DBLCOLON + N(TYU.SEN_S, TYPE_NAME) \
    + N(TYU.ARG_S, op(OPAR
                      + pp.delimitedList(VAR_OR_TYPE_DEC,
                                         TYU.PARAM_JOIN_S,
                                         combine=False)
                      + CPAR))

TYPEDEC_CORE.setParseAction(make_type_dec)



# NAMING
TYPEDEC_CORE.setName("TypeDeclarationStatement")

parse_point = TYPEDEC_CORE

def parseString(in_string):
    return parse_point.parseString(in_string)[:]
