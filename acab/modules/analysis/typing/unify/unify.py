#!/usr/bin/env python3
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.value import AcabValue, Sentence
from acab.core.parsing.consts import s, s_key
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)

from .. import exceptions as TE
from . import util

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

unify_enum = util.unify_enum

@dataclass
class UnifyLogic:
    """
    Component functions for Acab Unification
    """
    sieve           : List[Callable[[AT.Value, AT.Value, AT.CtxIns], unify_enum]]
    truncate        : Callable[[AT.Sentence, AT.Sentence], Tuple[AT.Sentence, AT.Sentence]]
    apply           : Callable[[AT.Sentence, AT.CtxIns], AT.Sentence]
    entry_transform : Optional[Callable[[AT.Sentence, AT.Sentence, AT.CtxIns], Tuple[AT.Sentence, AT.Sentence]]] = field(default=None)
    early_exit      : Optional[Callable[[AT.Sentence, AT.Sentence, AT.CtxIns], unify_enum]]                      = field(default=None)



# Main Class  #################################################################

@dataclass
class Unifier:
    """
    The Main Unifier class.
    Instance it with a logic set, then call on pairs of sentences
    """

    logic : UnifyLogic = field()

    def apply(self, sen, ctx):
        return self.logic.apply(sen, ctx)

    def __call__(self,
                 first: AT.Sentence,
                 second: AT.Sentence,
                 ctx:AT.CtxIns=None,
                 logic:UnifyLogic=None) -> AT.CtxIns:
        """
        Main Unify Routine.
        Unifies two sentences, using a UnifyLogic
        returns the unifying ctx.
        For each word pair  in the sentences,
        the logic sieve of handlers is tried
        """
        if logic is None:
            logic = self.logic

        ctx_prime = MutableContextInstance(None, ctx)
        with ctx_prime:
            if logic.early_exit is not None and logic.early_exit(first, second, ctx_prime) is unify_enum.END:
                return ctx_prime.finish()

            if logic.truncate is not None:
                first, second = logic.truncate(first, second)

            if logic.entry_transform is not None:
                first, second, _ = logic.entry_transform(first, second, ctx_prime)

            if len(first) != len(second):
                raise TE.AcabMiscTypingException("Unification length mismatch: {len(first)}, {len(second)}")

            for f_word,s_word in zip(first, second):
                result = None
                for sieve_fn in logic.sieve:
                    result = sieve_fn(f_word, s_word, ctx_prime)
                    if result is unify_enum.NA:
                        continue
                    elif result is unify_enum.NEXT_WORD:
                        break
                    elif result is unify_enum.END:
                        break

                if result is unify_enum.END:
                    break

        return ctx_prime.finish()
