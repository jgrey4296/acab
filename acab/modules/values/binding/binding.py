#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab import types as AT
from acab.interfaces import value as VI
from acab.interfaces import context as CI
from acab.error.semantic import AcabSemanticException
from acab.core.data import default_structure as DS
from acab.core.data.value import Sentence, AcabValue

def bind(val, bindings):
    if isinstance(val, VI.Sentence_i):
        return sen_bind(val, bindings)
    elif isinstance(val, VI.Value_i):
        return val_bind(val, bindings)
    else:
        raise AcabSemanticException("Unrecognised type attempted to bind: ", val)

def val_bind(val:AT.Value, bindings:dict[Any, Any]|AT.CtxIns) -> AT.Value:
    """ Data needs to be able to bind a dictionary
    of values to internal variables
    return modified copy
    """
    if val.is_var and val.value in bindings:
        assert(not val.params)
        # TODO this may be an unnecessary build
        return AcabValue.build(bindings[val.value])

    if not any([x.is_var for x in val.params]):
        return val

    bound_params = [bind(x, bindings) for x in val.params]
    return val.copy(params=bound_params)



def sen_bind(val:AT.Sentence, bindings:dict[Any, Any]|AT.CtxIns) -> AT.Sentence:
    """ Given a dictionary of bindings, reify the sentence,
    using those bindings.
    ie: a.b.$x with {x: blah} => a.b.blah
    return modified copy

    """
    assert(isinstance(bindings, (CI.ContextInstance_i, dict)))
    if val[0].key() in bindings and bindings[val[0].key()] is None:
        # early exit if nothing to do
        return val

    if val.is_var and val.key() in bindings and bindings[val.key()] is not None:
        # where the val is entirely replaced
        # `$x` bind {'x':a.b.c} -> a.b.c
        retrieved = bindings[val.key()]
        data_to_apply = val[0].data.copy()
        data_to_apply.update({DS.BIND: False})
        return retrieved.copy(data=data_to_apply)

    # Otherwise check individual words
    output = []
    for i, word in enumerate(val):
        # early expand if a plain node
        if not word.is_var:
            output.append(word)
            continue

        if not word.key() in bindings:
            output.append(word)
            continue

        # Sentence invariant: only word[0] can have an at_bind
        if word.is_at_var:
            assert(i == 0)
            retrieved = bindings[DS.AT_BIND + word.key()]
        else:
            retrieved = bindings[word.key()]


        if isinstance(retrieved, VI.Sentence_i) and len(retrieved) == 1:
            # Flatten len 1 sentences:
            # [$x] bind {x : [a] } -> [a]
            data_to_apply = word.data.copy()
            data_to_apply.update({DS.BIND: False})
            copied = retrieved[0].copy(data=word.data)
            output.append(copied)
        elif isinstance(retrieved, VI.Value_i):
            # Apply the variables data to the retrieval
            # *except* the variable annotation
            data_to_apply = word.data.copy()
            data_to_apply.update({DS.BIND: False})
            copied = retrieved.copy(data=data_to_apply)
            output.append(retrieved)
        else:
            raise AcabBasicException("Sentence Bind should only ever handle AcabValues")

    if len(output) == 1 and isinstance(output[0], VI.Sentence_i):
        output = output[0].words

    return Sentence.build(output,
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
