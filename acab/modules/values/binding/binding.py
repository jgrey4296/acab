#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from types import FunctionType

from acab import AcabConfig
from acab import types as AT
import acab.core.defaults.value_keys as DS
from acab.error.semantic import AcabSemanticException
from acab.interfaces import context as CI
from acab.interfaces import data as DI
from acab.interfaces import value as VI
from acab.interfaces.value import Sentence_i, Value_i
from acab.core.meta_classes.singletons import SingletonMeta
from acab.interfaces.bind import Bind_i

logging = logmod.getLogger(__name__)

__all__ = ['Bind']

config = AcabConfig()
bind_depth_max = config.attr.Binding.MAX_DEPTH
bind_total_max = config.attr.Binding.TOTAL_COUNT

# TODO make this a handler system, or part of semantics?

class Bind(Bind_i):

    recorded_depth : ClassVar[int] = 0
    total_call     : ClassVar[int] = 0

    @staticmethod
    def bind(val: VI.Value_i, bindings:Mapping):
        """
        Passed in a `val`, return it unchanged if its not a variable,
        if it is a variable, return the value it maps to
        """
        Bind.recorded_depth = 0
        Bind.total_call     = 0

        logging.debug("Binding: {} with {}", val, bindings)
        assert(isinstance(val, VI.Value_i)), val
        assert(hasattr(bindings, "__getitem__") or isinstance(bindings,CI.ContextInstance_i))

        result = _bind_top(val, bindings,)

        if Bind.recorded_depth > bind_depth_max:
            logging.warning("Binding Reached Depth {} for: {}", Bind.recorded_depth, val)

        if Bind.total_call > bind_total_max:
            logging.warning("Binding Totaled {} for: {}", Bind.total_call, val)

        return result

def _bind_top(val, bindings, depth=0):
    logging.info("Binding {} : Depth {} : {}", Bind.total_call, depth, val)
    Bind.total_call += 1
    if depth > Bind.recorded_depth:
        Bind.recorded_depth = depth

    match val:
        case DI.Node_i() | DI.StructView():
            result = val
        case VI.Sentence_i() if DS.OPERATOR in val.type and val.key() in bindings:
            result = bindings[val.key()]
        case VI.Sentence_i() if val.is_at_var:
            result = _bind_node(val[0], bindings)
        case VI.Sentence_i():
            words = [_bind_top(x, bindings, depth=depth+1) for x in val]
            result = val.copy(value=words)
            if any(DS.FLATTEN in x.data and x.data[DS.FLATTEN] for x in result):
                result = result.flatten()
            if val.is_var and len(words) == 1 and isinstance(words[0], VI.Sentence_i):
                result = words[0]
        case VI.Instruction_i() if val.type[:2] == "_:INSTRUCT.CONTAINER":
            try:
                masked = bindings.copy(mask=val.params)
            except TypeError:
                masked = {x:y for x,y in bindings.items() if x not in val.params}

            clauses = [_bind_top(x, masked,depth=depth+1) for x in val.clauses]
            result = val.copy(value=clauses)
        case VI.Instruction_i() if val.type[:2] == "_:INSTRUCT.STRUCT":
            try:
                masked = bindings.copy(mask=val.params)
            except TypeError:
                masked = {x:y for x,y in bindings.items() if x not in val.params}

            struct = {k: _bind_top(v, masked, depth=depth+1) for k,v in val.structure.items()}
            result = val.copy(structure=struct)
        case VI.Value_i() if val.is_at_var:
            result = _bind_node(val, bindings)
        case VI.Value_i() if val.is_var:
            initial = _bind_val(val, bindings, )
            # Bind its internals if necessary
            if initial != val:
                result = _bind_top(initial, bindings, depth=depth+1)
            else:
                result = initial
        case VI.Value_i():
            result = val

    return result

def _bind_val(val:VI.Value_i, bindings):
    # TODO recursive
    result = bindings[val.key()]
    match result:
        case str() if result == val.key():
            return val
        case VI.Value_i() if result.type == DS.OPERATOR_PRIM:
            # Operators don't get modified in any way
            return result
        case FunctionType() | DI.Node_i() | DI.StructView():
            return result
        case VI.Value_i() if result.is_var and result.key() in bindings:
            logging.warning("Binding returned an applicable variable: {} -> {} -> {}")

    # Run bind transforms here
    assert(isinstance(result, VI.Value_i)), result
    data_to_apply = val.data.copy()
    data_to_apply.update({DS.BIND: False})
    del data_to_apply[DS.TYPE_INSTANCE]
    result = result.copy(data=data_to_apply)

    return result

def _bind_node(val:VI.Value_i, bindings:AT.CtxIns) -> AT.Node:
    assert(val.is_at_var)
    assert(val in bindings)
    assert(val.key() in bindings.nodes)
    return bindings.nodes[val.key()]
