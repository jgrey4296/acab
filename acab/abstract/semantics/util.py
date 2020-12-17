#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic


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
