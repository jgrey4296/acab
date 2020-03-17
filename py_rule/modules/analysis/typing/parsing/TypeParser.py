"""
Pyparsing based parser for types
"""
import logging as root_logger
import pyparsing as pp
from py_rule.modules.analysis.typing.type_instance import TypeInstance
from py_rule.modules.analysis.typing import util as TYU
from py_rule.abstract.parsing import util as PU

def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[TYU.SEN_S][:]
    baseName = path[-1]
    args = []
    if TYU.ARG_S in toks:
        args = toks[TYU.ARG_S][:]
    elif (TYU.TYPE_DEC_S in baseName._data
          and baseName._data[TYU.TYPE_DEC_S] is not None):
        args.append(baseName._type)
        del baseName._data[TYU.TYPE_DEC_S]
    return (TYU.TYPE_DEC_S, TypeInstance(baseName, path, args))

# BASIC SENTENCE NEEDS TO BE POPULATED
# eg: py_rule.working_memory.trie_wm.parsing.FactParser.basic_fact_string
BASIC_SEN= pp.Forward()
TYPEDEC_CORE = pp.Forward()

TYPEDEC_CORE <<= PU.DBLCOLON + PU.N(TYU.SEN_S, BASIC_SEN) \
    + PU.N(TYU.ARG_S, PU.op(PU.OPAR
                      + pp.delimitedList(TYPEDEC_CORE,
                                         TYU.DELIM_S,
                                         combine=False)
                      + PU.CPAR))

TYPEDEC_CORE.setParseAction(make_type_dec)
