"""
Utilities and constructors for type parsing
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.defaults.value_keys as DS
import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.defaults import parse_keys as DK
from acab.core.parsing.annotation import ValueAnnotation
from acab.core.parsing.consts import s, s_key
from acab.interfaces import value as VI
from acab.modules.analysis.typing import util as TYU
from acab.modules.analysis.typing.values import (OperatorDefinition,
                                                 SumTypeDefinition,
                                                 TypeClass,
                                                 TypeDefinition)
VF = VI.ValueFactory

config          = AcabConfig()
TYPE_INSTANCE_S = DS.TYPE_INSTANCE

SEN_S           = DK.SEN
ARG_S           = DK.ARG
PARAM_JOIN_S    = config.prepare("Parse.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])()
SYNTAX_BIND_S   = config.prepare("Parse.Structure", "SYNTAX_BIND")()

NOM_HEAD        = s_key(config.prepare("Symbols", "NOMINAL")())
SUM_HEAD        = s_key(config.prepare("Symbols", "SUM")())
STRUCT_HEAD     = s_key(config.prepare("Symbols", "STRUCTURE")())
TYPE_CLASS_HEAD = s_key(config.prepare("Symbols", "TYPE_CLASS")())
FUNC_HEAD       = s(pp.Word(config.prepare("Symbols", "FUNC")()))

STRUCT_HEAD.set_name("StructHead")
FUNC_HEAD.set_name("FuncHead")


def make_simple_def(toks):
    value    = toks[0]
    val_data = value.data
    if DS.TYPE_INSTANCE in val_data and val_data[DS.TYPE_INSTANCE] == "ATOM":
        del val_data[DS.TYPE_INSTANCE]

    type_def = TypeDefinition([], name=value.name, params=[], data=val_data)
    return type_def

def make_record_def(toks):
    # check only atoms and typedefs are in body
    assert(all([isinstance(x, VI.Sentence_i) for x in toks[0]]))
    type_def = TypeDefinition([x for x in toks[0]])
    return type_def

def make_sum_def(toks):
    # assert all toks are sentences of atoms or type defs
    assert(all([isinstance(x, VI.Sentence_i) for x in toks[0]]))
    assert(all([isinstance(x[-1], TypeDefinition) for x in toks[0]]))
    sum_def = SumTypeDefinition(toks[0][:])
    return sum_def

def make_op_def(toks):
    syntax_bind = None
    specs = []
    # Unwrap the multi-def group if necessary
    target = toks[:] if 'params' in toks[0] else toks[0][:]

    for spec_group in target:
        assert(all([x.is_var for x in spec_group['params']]))
        param_sen = VF.sen() << (spec_group['params'][:] or "∅")
        ret_sen  = VF.sen() << 'returns' << (spec_group['returns'] if 'returns' in spec_group else "unit")
        joint_spec = VF.sen() << param_sen << ret_sen
        specs.append(joint_spec)

    op_def = OperatorDefinition(specs)

    return op_def


def make_type_dec(toks):
    """ Construct a type declaration / annotation
    Returns a Tuple signifying it is a type dec, and the type it annotates
    """
    path = toks[SEN_S]
    args = []
    if ARG_S in toks:
        args = [x[1] if isinstance(x, tuple) else x for x in toks[ARG_S][:]]

    return ValueAnnotation(TYU.TYPE_INSTANCE_S,
                           VI.ValueFactory.sen(path, params=args,
                                               data={TYPE_INSTANCE_S: "type.declaration"}))


def make_type_class(toks):
    # TODO assert all toks are operator definitions
    return TypeClass(toks[0][:])
