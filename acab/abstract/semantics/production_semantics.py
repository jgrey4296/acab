#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from re import Pattern
from uuid import uuid1, UUID
from weakref import ref
import logging as root_logger

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.semantics_interface import ProductionSemanticInterface
from acab.abstract.core.values import AcabValue, Sentence

config = AcabConfig.Get()


# TODO
@dataclass
class ProductionSemantics(ProductionSemanticInterface):
    """ Describes how abstractions are used """
    semantic_mapping: Dict[Sentence, Callable] = field(default_factory=dict)

    def __call__(self, obj, ctxs, engine, override=None):
        raise NotImplementedError()
        # Get semantic function for object
        # run it
        # return result
        pass


    def _initial_ctx_construction(self, ctxs: List[Dict[Any, Any]]) -> List[Dict[Any, Any]]:
        query_result = [{}]
        if ctxs is not None and bool(ctxs):
            query_result = [x.copy() for x in ctxs]

        return query_result

    def _param_ctx_filter(self, params, ctxs):
        # verify params -> ctxs fit
        result = []
        param_set = params
        for ctx_singular in ctxs:
            ctx_set = ctx_singular
            if param_set in ctx_set:
                result.append(ctx_singular)

        return result


    def _get_params(self, obj, data):
        assert(isinstance(data, dict))
        output = []
        # TODO: enable currying
        for x in self.params:
            if isinstance(x, Sentence):
                output.append(x.bind(data))
            elif isinstance(x, list):
                output.append([y.bind(data) for y in x])
            elif isinstance(x, AcabValue) and x.is_var:
                assert(x.value in data)
                if x.is_at_var:
                    output.append(data[AT_BIND_S + x.value])
                elif isinstance(data[x.value], list):
                    # TODO does this need to unwrap all list values?
                    output.append(data[x.value])
                else:
                    output.append(data[x.value].value)
            else:
                output.append(x.value)
        return output


