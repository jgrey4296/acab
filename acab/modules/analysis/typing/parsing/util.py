"""
Utilities and constructors for type parsing
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.core.config.config import AcabConfig
from acab.core.data.sentence import Sentence
from acab.modules.analysis.typing import util as TYU
from acab.modules.analysis.typing.values.definition import (
    OperatorDefinition, SumTypeDefinition, TypeDefinition, TypeClass)

from acab.core.parsing.annotation import ValueAnnotation

config          = AcabConfig()
TYPE_INSTANCE_S = config.prepare("Value.Structure", "TYPE_INSTANCE")()

def make_simple_def(toks):
    value    = toks[0]
    type_def = TypeDefinition([], name=value.name, params=[], data=value.data)
    return type_def

def make_record_def(toks):
    # check only atoms and typedefs are in body
    assert(all([isinstance(x, Sentence) for x in toks[:]]))
    type_def = TypeDefinition(toks[:])
    return type_def

def make_sum_def(toks):
    # assert all toks are sentences of atoms or type defs
    assert(all([isinstance(x, Sentence) for x in toks[:]]))
    assert(all([isinstance(x[-1], TypeDefinition) for x in toks[:]]))
    sum_def = SumTypeDefinition(toks[:])
    return sum_def

def make_op_def(toks):
    syntax_bind = None
    op_params = toks["params"]
    assert(all([x.is_var for x in op_params]))

    if TYU.SYNTAX_BIND_S in toks:
        syntax_bind = toks[TYU.SYNTAX_BIND_S]

    op_def = OperatorDefinition([op_params], sugar_syntax=syntax_bind)

    return op_def


def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[TYU.SEN_S]
    args = []
    if TYU.ARG_S in toks:
        args = [x[1] if isinstance(x, tuple) else x for x in toks[TYU.ARG_S][:]]

    return ValueAnnotation(TYU.TYPE_INSTANCE_S,
                           Sentence(path, params=args,
                                    data={TYPE_INSTANCE_S: "type.declaration"}))

def make_type_class(toks):
    # TODO assert all toks are operator definitions
    return TypeClass(None, toks[:])
