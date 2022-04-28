#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT
from acab.core.value import default_structure as DS
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.error.semantic import AcabSemanticException
from acab.interfaces import context as CI
from acab.interfaces import data as DI
from acab.interfaces import value as VI
from acab.interfaces.value import Sentence_i, Value_i

logging = logmod.getLogger(__name__)

# TODO make this a handler system, or part of semantics?

def bind(val, bindings, semSys=None):
    """
    Passed in a `val`, return it unchanged if its not a variable,
    if it is a variable, return the value it maps to
    """
    logging.debug("Binding: {} with {}", val, bindings)
    result = __bind(val, bindings, semSys)

    # Only flatten the top most sentence returned, as it will recurse if necessary
    if isinstance(result, Sentence_i):
        result = result.flatten()

    return result

def __bind(val, bindings, semSys=None):
    if not isinstance(bindings, list):
        bindings = [bindings]

    assert(all([isinstance(x, CI.ContextInstance_i) for x in bindings]))
    bindings = bindings[:]
    result = val
    while result is val and bool(bindings):
        current = bindings.pop()
        match val:
            case VI.Sentence_i() if DS.OPERATOR in val.data:
                result = current[str(val)]
            case VI.Value_i() if not val.has_var:
                result = val
            case VI.Sentence_i(), val.is_var:
                result = bind_val(val[0])
            case VI.Sentence_i():
                result = sen_bind(val, current)
            case VI.Value_i():
                result = bind_val(val, current)
            case _:
                raise AcabSemanticException("Unrecognised type attempted to bind: ", val)

    if result is val:
        return val
    if DS.OPERATOR in val.data:
        return result

    # Run top level bind transforms here
    data_to_apply = val.data.copy()
    data_to_apply.update({DS.BIND: False})
    # TODO may need to remove type if its atom too
    result = result.copy(data=data_to_apply)
    # Bind parameters
    if any([x.is_var for x in result.params]):
        bound_params = [__bind(x, bindings) for x in result.params]
        result       = result.copy(params=bound_params)

    return result


def bind_val(val:AT.Value, bindings:AT.CtxIns) -> AT.Value:
    """ Data needs to be able to bind a dictionary
    of values to internal variables
    return modified copy
    """
    assert(val.has_var)
    bound = val
    match val:
        case _ if val.value not in bindings:
            pass
        case val.is_at_var:
            bound = bindings.nodes[val.value]
            assert(isinstance(bound, DI.Node_i))
        case _:
            bound = bindings[val.value]
            assert(isinstance(bound, VI.Value_i))

    return bound


def sen_bind(val:AT.Sentence, bindings:AT.CtxIns) -> AT.Sentence:
    """ Given a ctxinstance of bindings, reify the sentence,
    using those bindings.
    ie: a.b.$x with {x: blah} => a.b.blah
    return modified copy
    """
    assert(val.has_var)
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
                output.append(__bind(word, bindings))
            case _:
                output.append(__bind(word, bindings))

    return Sentence(output,
                    data=val.data,
                    params=val.params,
                    tags=val.tags)


def production_component_bind(val, data) -> AT.Component:
    # Bind params / operator
    if val.op.is_var and val.op.value in data:
        bound_op = data[val.op.value]
    else:
        bound_op = val.op.copy()

    bound_params = [x.bind(data) for x in val.params]

    return val.copy(value=bound_op, params=bound_params)


def production_container_bind(val, data) -> AT.Container:
    # Bind params,
    # then Bind each clause separately,
    bound_clauses = [x.bind(data) for x in val.value]
    bound_params  = [x.bind(data) for x in val.params]
    return val.copy(value=bound_clauses, params=bound_params)

def production_structure_bind(val, data) -> AT.ProductionStructure:
    # Bind params,
    bound_params  = [bind(x, data) for x in val.params]
    # Bind clauses
    bound_clauses = [bind(x, data) for x in val.clauses]
    # Bind sub containers
    bound_struct  = {x: bind(y, data) for x,y in val.structure.items()}

    return val.copy(value=bound_clauses, params=bound_params, structure=bound_struct)
