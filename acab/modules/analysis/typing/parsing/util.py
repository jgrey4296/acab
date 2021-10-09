"""
Utilities and constructors for type parsing
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.core.config.config import AcabConfig
from acab.core.data.values import Sentence
from acab.modules.analysis.typing import util as TYU
from acab.modules.analysis.typing.values.type_definition import (
    OperatorDefinition, SumTypeDefinition, TypeDefinition, TypeClass)

from acab.core.parsing.annotation import ValueAnnotation

config          = AcabConfig.Get()
TYPE_INSTANCE_S = config.prepare("Value.Structure", "TYPE_INSTANCE")()

def make_simple_def(toks):
    value= toks[0]
    type_def = TypeDefinition(None, [])
    return ealue.attach_statement(type_def)

def make_record_def(toks):
    type_def = TypeDefinition(None, toks[:])
    return type_def

def make_op_def(toks):
    syntax_bind = None
    if TYU.SYNTAX_BIND_S in toks:
        syntax_bind = toks[TYU.SYNTAX_BIND_S][0]

    op_def = OperatorDefinition(None, [toks[TYU.STRUCT_S][0]], sugar_syntax=syntax_bind)

    return op_def

def make_sum_def(toks):
    sum_def = SumTypeDefinition(None, toks[:])
    return sum_def


def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[TYU.SEN_S]
    args = []
    if TYU.ARG_S in toks:
        args = [x[1] if isinstance(x, tuple) else x for x in toks[TYU.ARG_S][:]]

    return ValueAnnotation(TYU.TYPE_INSTANCE_S,
                           Sentence.build(path, params=args,
                                          data={TYPE_INSTANCE_S: "_:type.declaration"}))

def make_type_class(toks):
    return TypeClass(None, toks[:])
