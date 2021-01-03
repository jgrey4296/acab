#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from enum import Enum

Printable = Union['AcabValue', 'AcabNode', 'AcabStruct', 'Contexts', str]
# pylint: disable       =line-too-long
# AcabValue -> Value(Op), Statement(Sentence, Component) Container(Query, Transform, Action), Structured:(Rule, Agenda, Layer, Pipeline)
# pylint: enable        =line-too-long

RET_enum                = Enum(
    "HandlerEnum", "PASS SIMPLE ACCUMULATOR SUBSTRUCT CALL SENTINEL PRINTABLE"
)
SEARCH_enum             = Enum("Semantic Searches", "UUID NAME VALUE ACAB_T PY_T THROW")


AccumulationDict        = Dict[Any, Any]
AccumulatorReturn       = Tuple[RET_enum, Dict[str, Any], None]
SubstructReturn         = Tuple[RET_enum, List[Printable], Optional["Sentinel"]]
SimpleReturn            = Tuple[RET_enum, str, None]
HandlerReturnUnion      = Union[AccumulatorReturn, SubstructReturn, SimpleReturn]

Handler                 = Callable[
    ["AcabPrintSemantics", Printable, AccumulationDict, Any], HandlerReturnUnion
]
Sentinel                = Callable[
    ["AcabPrintSemantics", Printable, List[str], AccumulationDict, Any],
    HandlerReturnUnion,
]

SemanticSpec  = Tuple[List[Handler], Sentinel]
ContextValue  = str
SemBox        = Tuple[RET_enum, Printable, Callable]
StackValue    = Tuple[List[SemBox], List[str], Dict[Any, Any]]


# TODO Utils to use in semantic mapping
def _get_by_uuid(printer, val: Printable) -> SemanticSpec:
        if val.uuid in printer._type_semantics:
            return printer._type_semantics[val.uuid]

        return None

def _get_by_name(printer, val: Printable) -> Optional[SemanticSpec]:
        if val.name in printer._type_semantics:
            return printer._type_semantics[val.name]

        return None

def _get_by_value(printer, val: Printable) -> Optional[SemanticSpec]:
        if val.value in printer._type_semantics:
            return printer._type_semantics[val.name]

        return None

def _get_by_acab_type_hierarchy(printer, val) -> Optional[SemanticSpec]:
        acab_types = [val.type]

        while bool(acab_types):
            current = acab_types.pop(0)
            if current in printer._type_semantics:
                return printer._type_semantics[current]

        return None

def _get_by_python_type_hierarchy(printer, val) -> Optional[SemanticSpec]:
        types = [val.__class__]
        while bool(types):
            current = types.pop(0)
            if current in printer._type_semantics:
                return printer._type_semantics[current]
            elif current.__base__ is not object:
                types.append(current.__base__)


        return None


def _throw_semantic_search(printer, val):
    raise AcabSemanticException(str(val), val)




def _compare(word, node,  ctxs=None, engine=None):
    # Get op from engine
    op = engine.get_operator(query_component.op)
    # AcabNode -> AcabValue -> Actual Value
    node_value = node.value.value
    params = ProdSem.get_params(data)

    return op(node_value, *params, data=data)


def default_enum_lookup():
    initial = {x: _throw_semantic_search for x in SEARCH_enum}
    initial[SEARCH_enum.UUID] = _get_by_uuid
    initial[SEARCH_enum.NAME] = _get_by_name
    initial[SEARCH_enum.VALUE] = _get_by_value
    initial[SEARCH_enum.ACAB_T] = _get_by_acab_type_hierarchy
    initial[SEARCH_enum.PY_T] = _get_by_python_type_hierarchy

    return initial

def init_struct(semantics, struct):
        """
        Create a basic root node / entry point for a data structure
        """
        constructor = semantics.lifting[AcabValue]
        node_semantics = semantics.retrieve_semantics(constructor)
        node = constructor.Root()
        return node_semantics.up(node)

def value_constructor(semantics, value):
        """
        Get the most applicable lifting from value -> node
        """
        assert(isinstance(value, type))
        # TODO should I be using my type instances for semantics?
        curr = value
        retrieved = None
        descendents_to_update = []
        while retrieved is None and curr not in (object, None):
            if curr in semantics.value_pairings:
                retrieved = semantics.value_pairings[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)


        if retrieved is None:
            raise AcabSemanticException("Missing Construction data for: {}".format(value),
                                        None)

        if len(descendents_to_update) > 1:
            semantics.value_pairings.update({x : retrieved for x in descendents_to_update})

        return retrieved

def rebind_across_contexts(names, values, base):
    """ Utility method to perform alpha conversion """
    assert(isinstance(base, dict))
    assert(isinstance(names, list))
    assert(isinstance(values, tuple))
    new_base = {}
    new_base.update(base)
    for x, y in zip(names, values):
        new_base[x.name] = AcabValue.safe_make(y)

    return new_base
