"""
Utilities and constructors for type parsing
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence
from acab.modules.analysis.typing.values.operator_definition import \
    OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import (
    SumTypeDefinition, TypeDefinition)
from acab.modules.analysis.typing import util as TYU

config          = AcabConfig.Get()
TYPE_INSTANCE_S = config.prepare("Value.Structure", "TYPE_INSTANCE")()

def make_record_def(toks):
    type_def = TypeDefinition(toks[:])
    return (TYPE_INSTANCE_S, type_def)

def make_op_def(toks):
    syntax_bind = None
    if TYU.SYNTAX_BIND_S in toks:
        syntax_bind = toks[TYU.SYNTAX_BIND_S][0]

    op_def = OperatorDefinition(toks[TYU.STRUCT_S][0], sugar_syntax=syntax_bind)

    return (TYPE_INSTANCE_S, op_def)

def make_sum_def(toks):
    sum_def = SumTypeDefinition(toks[:])
    return (TYPE_INSTANCE_S, sum_def)


def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[TYU.SEN_S]
    args = []
    if TYU.ARG_S in toks:
        args = [x[1] if isinstance(x, tuple) else x for x in toks[TYU.ARG_S][:]]
    return (TYPE_INSTANCE_S, Sentence.build(path, params=args))
