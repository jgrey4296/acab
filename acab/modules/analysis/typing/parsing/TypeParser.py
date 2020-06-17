"""
Pyparsing based parser for types
"""
import logging as root_logger
import pyparsing as pp
from acab.modules.analysis.typing.values.type_instance import TypeInstance
from acab.modules.analysis.typing import util as TYU
from acab.abstract.parsing import util as PU

def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[TYU.SEN_S]
    args = []
    if TYU.ARG_S in toks:
        args = toks[TYU.ARG_S][:]
    return (TYU.TYPE_DEC_S, TypeInstance(path, args))

# BASIC SENTENCE NEEDS TO BE POPULATED
# eg: acab.working_memory.trie_wm.parsing.FactParser.basic_fact_string
HOTLOAD_BASIC_SEN= pp.Forward()
TYPEDEC_CORE = pp.Forward()

VAR_OR_TYPE_DEC = pp.Or([PU.BIND, TYPEDEC_CORE])

TYPEDEC_CORE <<= PU.DBLCOLON + PU.N(TYU.SEN_S, HOTLOAD_BASIC_SEN) \
    + PU.N(TYU.ARG_S, PU.op(PU.OPAR
                      + pp.delimitedList(VAR_OR_TYPE_DEC,
                                         TYU.DELIM_S,
                                         combine=False)
                      + PU.CPAR))

TYPEDEC_CORE.setParseAction(make_type_dec)

# NAMING
TYPEDEC_CORE.setName("TypeDeclarationStatement")
