#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from re import Pattern
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Protocol,
                    Sequence, Set, Tuple, TypeAlias, TypeVar, Union, cast)
from uuid import UUID, uuid1
from weakref import ref

logging = logmod.getLogger(__name__)

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.util import name_sieve_fns
from acab.core.decorators.util import cache
from acab.core.util.singletons import SingletonMeta
from acab.error.base import AcabBasicException
from acab.interfaces import context as CtxInt
from acab.interfaces.sieve import AcabSieve
from acab.modules.context.context_instance import ContextInstance

logging        = logmod.getLogger(__name__)

config         = AcabConfig()
BIND_SYMBOL    = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP      = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T              = TypeVar('T', bound=AT.ValueCore)
C              = TypeVar('C', bound=type)

Value_A       : TypeAlias = "AT.Value[AT.ValueCore_t]"
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = str

ProtocolMeta = type(Protocol)

class ContextMeta(ProtocolMeta):
    """ Utility Meta Class for building values """

    name_sieve           : ClassVar[AcabSieve[str]] = AcabSieve(name_sieve_fns)
    default_data         : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE : DS.TYPE_BASE,
                                                       DS.BIND : False}

    _bottom       : ClassVar[None|type[Value_A]]           = None
    __subclasses  : ClassVar[dict[str,type[Value_A]]]      = dict()

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ContextMeta, cls).__init__(name, bases, data)

    def __call__(cls, ops:None|CtxIns|list[ModuleComponents]=None, **kwargs):
        """
        The Meta Constructor for ContextSets,
        to construct operator bindings if necessary
        """
        if ops is None:
            return super(ContextMeta, cls).__call__(**kwargs)

        assert('_operators' not in kwargs)
        if isinstance(ops, CtxInt.ContextInstance_i):
            kwargs['_operators'] = ops
            return super(ContextMeta, cls).__call__(**kwargs)

        assert(isinstance(ops, list)), ops
        # Get Flat List of Operator Sentences:
        operators = [y for x in ops for y in x.operators]
        # Build the CtxInst data dict:
        op_dict = {str(x) : x[-1] for x in operators}
        # Add any sugar forms:
        op_dict.update({x[-1]._acab_operator_sugar : x[-1] for x in operators if hasattr(x[-1], "_acab_operator_sugar")})

        # TODO abstract building ctxinst's to the set
        instance = ContextInstance(op_dict, exact=True)
        # TODO add sugar names from config

        # Build the actual value
        kwargs['_operators'] = instance
        new_obj = super(ContextMeta, cls).__call__(**kwargs)
        return new_obj
