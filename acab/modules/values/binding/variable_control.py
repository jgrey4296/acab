#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from types import FunctionType
from uuid import uuid1

from acab import types as AT
import acab.core.defaults.value_keys as DS
from acab.error.semantic import AcabSemanticException
from acab.interfaces import context as CI
from acab.interfaces import data as DI
from acab.interfaces import value as VI
from acab.interfaces.value import Sentence_i, Value_i
from acab.core.metaclasses.singletons import SingletonMeta
from acab.interfaces.bind import Bind_i

logging = logmod.getLogger(__name__)

__all__ = ['rectx']

def rectx(val, *, ctx=None, name_suff=None):
    """
    Take a value and a ctx,
    and substitute new variables for all variables in the val.

    Thus:
    a.$b.$c  -> a.$b_1.$c_1

    Provides a clean set of variables to avoid conflicts in unification

    """
    assert(isinstance(ctx, (CI.ContextInstance_i, dict)))
    name_suff = name_suff or uuid1()

    if val.type.has_var:
        data = {DS.TYPE_INSTANCE: rectx(val.type, ctx=ctx, name_suff=name_suff)}
    else:
        data = {}

    params = _unique_params(val.params, ctx, name_suff)
    match val:
        case VI.Sentence_i():
            # recurse down
            words = [rectx(x, ctx=ctx, name_suff=name_suff) for x in val.words]
            return val.copy(value=words, params=params, data=data)
        case VI.Instruction_i():
            # TODO work on this
            return val.copy(params=params, data=data)
        case VI.Value_i() if val.is_var and val.key() not in ctx:
            # Create a new var
            uniq = f"{val.key()}_{name_suff}"
            ctx[val.key()] = val.copy(name=uniq,
                                      params=params,
                                      data=data)
            # link it to the original by uuid
            return ctx[val.key()]
        case VI.Value_i() if val.is_var and val.key() in ctx:
            return val.copy(name=ctx[val.key()].key(),
                            params=params,
                            data=data)
        case _ if bool(data):
            assert(not val.is_var)
            assert(not val.has_var)
            return val.copy(data=data)
        case _:
            return val

def _unique_params(params:list[VI.Value_i], ctx, name_suff):
    new_params = []
    for param in params:
        assert(param.is_var)
        match param:
            case _ if param.key() not in ctx:
                uniq = f"{param.key()}_{name_suff}"
                ctx[param.key()] = param.copy(name=uniq)
                new_params.append(ctx[param.key()])
            case _ if param.key in ctx:
                new_params.append(param.copy(name=ctx[param.key()].key()))

    return new_params
