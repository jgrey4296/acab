#!/usr/bin/env python3
import logging as logmod
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

import acab.core.util.part_implementations.sentence as SSI  # type:ignore
import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.value.value_meta import ValueMeta
from acab.error.base import AcabBasicException
from acab.error.protocol import AcabProtocolError as APE
from acab.error.semantic import AcabOperatorMissingException
from acab.interfaces.value import ValueFactory

logging        = logmod.getLogger(__name__)

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
@dataclass(frozen=True, repr=False, eq=False)
class Sentence(SSI.SentenceProtocolsImpl, VI.Sentence_i, metaclass=ValueMeta):
    """
    A Sentence is an instruction which is idempotent on from_sentences/to_sentences
    Sentence.from_sentences([sens]) == [sens]
    """
    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.SENTENCE_PRIM, DS.NEGATION: False}

    @classmethod
    def _preprocess(cls, *args, **kwargs):
        value = args[0] or []
        assert(isinstance(value, Iterable))
        processed = [VI.ValueFactory.value(x) if not isinstance(x, VI.Value_i) else x for x in value]
        return processed

    def __post_init__(self):
        if self.data[DS.BIND] != False:
            raise TypeError("Sentences Shouldn't be variables")

    def match(self, sen:Sen_A) -> list[Tuple[Value_A, Value_A]]:
        """ Match a target sentence's variables to self's target
        as tuples of (bind_name, value)

        eg: _:$x.b.$y match _:a.b.c -> [(x, a), (y, c)]
        """
        raise DeprecationWarning()
        # TODO this should be a controllable / utility function
        results : list[Tuple[Value_A, Value_A]] = []
        if self.is_var:
            results.append((self[0], sen))
            return results

        for x,y in zip(self.words, sen.words): #type:ignore
            if x.is_var:
                results.append((x,y))

        return results

    def __lshift__(self, other):
        """
        For easy programmatic creation of sentences:

        a_sen = Sentence(data=...) << "a" << "test" << ["sentence", "also", "handles", "lists"]
        a_sen == "_:a.test.sentence.also.handles.lists"
        """
        if other is None or not bool(other):
            return self
        if not isinstance(other, list):
            other = [other]
        words = self.words + other
        return self.copy(value=words)

    def __call__(self, *args, **kwargs):
        raise AcabOperatorMissingException("Attempted to call a sentence, likely instead of an operator", context=[self])
