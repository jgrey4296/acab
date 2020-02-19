"""
Pyparsing based parser for types
"""
import logging as root_logger
import pyparsing as pp
from py_rule.typing.type_instance import TypeInstance
from py_rule.typing import util as TU
from py_rule.abstract.parsing import util as PU


def make_type_dec(toks):
    path = toks.SEN[:]
    baseName = path[-1]
    args = []
    if 'ARGS' in toks:
        args = toks.ARGS[:]
    elif (TU.TYPE_DEC_S in baseName._data
          and baseName._data[TU.TYPE_DEC_S] is not None):
        args.append(baseName._type)
        del baseName._data[TU.TYPE_DEC_S]
    return (TU.TYPE_DEC_S, TypeInstance(baseName, path, args))


# BASIC SENTENCE NEEDS TO BE POPULATED
# eg: py_rule.knowledge_base.trie_kb.parsing.FactParser.basic_fact_string
BASIC_SENTENCE = pp.Forward()
TYPEDEC_CORE = pp.Forward()

TYPEDEC_CORE <<= PU.DBLCOLON + PU.N("SEN", BASIC_SENTENCE) \
    + PU.N("ARGS", PU.op(PU.OPAR
                         + pp.delimitedList(TYPEDEC_CORE,
                                            ', ',
                                            combine=False)
                         + PU.CPAR))

TYPEDEC_CORE.setParseAction(make_type_dec)
