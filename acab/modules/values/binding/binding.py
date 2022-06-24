#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from types import FunctionType

from acab import types as AT
import acab.core.defaults.value_keys as DS
from acab.error.semantic import AcabSemanticException
from acab.interfaces import context as CI
from acab.interfaces import data as DI
from acab.interfaces import value as VI
from acab.interfaces.value import Sentence_i, Value_i
from acab.core.util.singletons import SingletonMeta
from acab.interfaces.bind import Bind_i

logging = logmod.getLogger(__name__)

__all__ = ['Bind']

# TODO make this a handler system, or part of semantics?

class Bind(Bind_i):

    @staticmethod
    def bind(val, bindings, semSys=None):
        """
        Passed in a `val`, return it unchanged if its not a variable,
        if it is a variable, return the value it maps to
        """
        logging.debug("Binding: {} with {}", val, bindings)
        assert(isinstance(val, VI.Value_i))
        if not isinstance(bindings, list):
            bindings = [bindings]

        result = _bind(val, bindings, semSys=semSys)
        # Only flatten the top most sentence returned, as it will recurse if necessary
        if isinstance(result, Sentence_i):
            result = result.flatten()

        match result:
            case DI.Node_i() | DI.StructView():
                pass
            case VI.Sentence_i():
                words = [Bind.bind(x, bindings, semSys) for x in result]
                result = result.copy(value=words).flatten()
            case VI.Instruction_i() if result.type[:2] == "_:INSTRUCT.CONTAINER":
                masked = bindings[0].copy(mask=result.params)
                clauses = [Bind.bind(x, masked, semSys) for x in result.clauses]
                result = result.copy(value=clauses)
            case VI.Instruction_i() if result.type[:2] == "_:INSTRUCT.STRUCT":
                masked = bindings[0].copy(mask=result.params)
                struct = {k: Bind.bind(v, masked, semSys) for k,v in result.structure.items()}
                result = result.copy(structure=struct)

        return result


def _bind(val, bindings, semSys=None):
    if not isinstance(bindings, list):
        bindings = [bindings]

    assert(all([isinstance(x, CI.ContextInstance_i) for x in bindings]))
    bindings = bindings[:]
    result = val
    while result is val and bool(bindings):
        current = bindings.pop()
        match val:
            case VI.Sentence_i() if val.is_at_var:
                return _bind_node(val[0], current)
            case VI.Value_i() if val.is_at_var:
                return _bind_node(val, current)
            case VI.Sentence_i():
                result = _sen_bind(val, current)
            case VI.Value_i():
                result = _bind_val(val, current)
            case _:
                raise AcabSemanticException("Unrecognised type attempted to bind: ", val)

    match result:
        case _ if val is result:
            # nothing was bound, so early exit
            return val
        case VI.Value_i() if result.type == DS.OPERATOR_PRIM:
            # Operators don't get modified in any way
            return result
        case FunctionType() | DI.Node_i() | DI.StructView():
            return result

    # Run bind transforms here
    assert(isinstance(result, VI.Value_i)), result
    data_to_apply = val.data.copy()
    data_to_apply.update({DS.BIND: False})
    del data_to_apply[DS.TYPE_INSTANCE]
    # TODO may need to remove type if its atom too
    result = result.copy(data=data_to_apply)
    # Bind parameters
    if any([x.is_var for x in result.params]):
        bound_params = [_bind(x, bindings) for x in result.params]
        result       = result.copy(params=bound_params)

    return result


def _bind_val(val:AT.Value, bindings:AT.CtxIns) -> AT.Value:
    """ Data needs to be able to bind a dictionary
    of values to internal variables
    return modified copy
    """
    if not val.is_var:
        return val

    bound = val
    match val:
        case _ if val.value not in bindings:
            pass
        case _:
            bound = bindings[val.value]
            assert(isinstance(bound, VI.Value_i))

    return bound


def _sen_bind(val:AT.Sentence, bindings:AT.CtxIns) -> AT.Sentence:
    """ Given a ctxinstance of bindings, reify the sentence,
    using those bindings.
    ie: a.b.$x with {x: blah} => a.b.blah
    return modified copy
    """
    if DS.OPERATOR in val.type and str(val) in bindings:
        return bindings[str(val)]

    output = []
    # Sentence invariant: only word[0] can have an at_bind
    for i, word in enumerate(val):
        # early expand if a plain node
        # TODO could flatten retrieved here potentially
        match word:
            case _ if not (word.is_var or word in bindings):
                output.append(word)
                continue
            case _ if word.is_at_var:
                assert(i == 0)
                output.append(Bind.bind(word.copy(data={DS.BIND: True}), bindings))
            case _:
                output.append(Bind.bind(word, bindings))

    return val.copy(value=output)


def _bind_node(val:VI.Value_i, bindings:AT.CtxIns) -> AT.Node:
    assert(val.is_at_var)
    assert(val in bindings)
    assert(val.key() in bindings.nodes)
    return bindings.nodes[val.key()]
