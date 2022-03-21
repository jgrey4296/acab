#!/usr/bin/env python3
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, Type,
                    TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.core.data.default_structure as DS
import acab.core.data.sub_implementations.sentence as SSI  # type:ignore
import acab.core.data.sub_implementations.value as VSI
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.util import name_sieve_fns
from acab.core.data.value import AcabValue
from acab.core.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve
from acab.error.protocol import AcabProtocolError as APE
from acab.core.data.factory import ValueFactory
from acab.core.data.value_meta import ValueMeta

logging        = root_logger.getLogger(__name__)

config         = AcabConfig()
BIND_SYMBOL    = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP      = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T              = TypeVar('T', bound=AT.ValueCore)

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = str

@APE.assert_implements(VI.Sentence_i)
class Sentence(SSI.SentenceProtocolsImpl, VI.Sentence_i, metaclass=ValueMeta):
    """
    A Sentence is an instruction which is idempotent on from_sentences/to_sentences
    Sentence.from_sentences([sens]) == [sens]
    """
    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.SENTENCE_PRIM, DS.NEGATION: False}

    @classmethod
    def _preprocess(cls, *args, **kwargs):
        value = args[0]
        assert(isinstance(value, Iterable))
        return [ValueMeta._bottom(x) for x in value]

    def match(self, sen:Sen_A) -> list[Tuple[Value_A, Value_A]]:
        """ Match a target sentence's variables to self's target
        as tuples of (bind_name, value)

        eg: _:$x.b.$y match _:a.b.c -> [(x, a), (y, c)]
        """
        # TODO this should be a controllable / utility function
        results : list[Tuple[Value_A, Value_A]] = []
        if self.is_var:
            results.append((self[0], sen))
            return results

        for x,y in zip(self.words, sen.words): #type:ignore
            if x.is_var:
                results.append((x,y))

        return results

    def do_break(self) -> None: pass

    @property
    def should_break(self) -> bool:
        return bool(self.breakpoint)
