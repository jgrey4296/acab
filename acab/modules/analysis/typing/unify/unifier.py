#!/usr/bin/env python3
from __future__ import annotations
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.value.instruction import Instruction
from acab.core.parsing.consts import s, s_key
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_instance import (ContextInstance,
                                                   MutableContextInstance)

from .. import exceptions as TE
from . import util

logging = logmod.getLogger(__name__)

config = AcabConfig()

unify_enum = util.unify_enum

@dataclass
class UnifyLogic:
    """
    Component functions for Acab Unification
    """
    sieve           : list[Callable[[AT.Value, AT.Value, AT.CtxIns], unify_enum]]
    truncate        : Callable[[AT.Sentence, AT.Sentence], Tuple[AT.Sentence, AT.Sentence]]
    apply           : Callable[[AT.Sentence, AT.CtxIns], AT.Sentence]
    entry_transform : None | Callable[[AT.Sentence, AT.Sentence, AT.CtxIns], Tuple[AT.Sentence, AT.Sentence]] = field(default=None)
    early_exit      : None | Callable[[AT.Sentence, AT.Sentence, AT.CtxIns], unify_enum]                      = field(default=None)
    sub_logic       : None | UnifyLogic = field(default=None)



# Main Class  #################################################################

@dataclass
class Unifier:
    """
    The Main Unifier class.
    Instance it with a logic set, then call on pairs of sentences
    """

    logic : UnifyLogic = field()

    def apply(self, sen, ctx):
        """ Apply substitutions in a context instance onto a sentence """
        return self.logic.apply(sen, ctx)

    def __repr__(self):
        return f"<Unifier>"

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
        logging.debug("Starting Unify for: {}, {}", first, second)
        logic = logic or self.logic

        # TODO first, second : (list[Sentences], list[Sentence])
        # TODO add exhaustive / inclusive typing options
        ctx_prime = MutableContextInstance(None, ctx)
        with ctx_prime:
            # PREPARATION AND EARLY EXIT #######################################
            if (logic.early_exit is not None and
                logic.early_exit(first, second, ctx_prime, unifier=self) is unify_enum.END):
                logging.debug("Early Exit Success")
                raise ctx_prime.EarlyExitException()

            if logic.truncate is not None:
                first, second = logic.truncate(first, second)

            if logic.entry_transform is not None:
                first, second, _ = logic.entry_transform(first, second, ctx_prime)

            if len(first) != len(second):
                raise TE.AcabLengthUnifyException(first, second)

            logging.debug("Unify Setup Complete, running loop")
            # MAIN UNIFY LOOP #################################################
            try:
                for index in range(len(first)):
                    result = None
                    f_word  = first[index]
                    s_word  = second[index]

                    for sieve_fn in logic.sieve:
                        logging.debug("Running {:<20} on {!s:<10}, {!s:<10}",
                                      sieve_fn.__name__, f_word, s_word)
                        result = sieve_fn(index, first, second, ctx_prime, unifier=self)
                        match result:
                            case unify_enum.NA:
                                continue
                            case unify_enum.NEXT_WORD:
                                break
                            case unify_enum.END:
                                raise ctx_prime.EarlyExitException()

                    logging.debug("---")

            except TE.AcabTypingException as err:
                # insert the sentences into the error for easier debugging
                err.data.update({'left_sentence': first, 'right_sentence': second})
                raise err from err

        return ctx_prime.final_ctx


    def repeat(self,
               first:set[AT.Sentence],
               second:set[AT.Sentence] | 'Trie',
               ctx:AT.CtxIns,
               logic=None):
        """
        Unify all sentences in a set (`first`)
        against any applicable sentence in another set (`second`)
        TODO: `second` could be made into a trie
        """

        if logic is None:
            logic = self.logic

        # TODO error if first doesn't have one head.
        # ie: a.b.c, q.b.c should error

        the_ctx = ctx
        failures = []
        for fst in first:
            unified = False
            for snd in second:
                try:
                    # unify a first with any of second
                    ret_ctx = self(fst, snd, the_ctx)
                    # and update the ctx used as master
                    the_ctx = ret_ctx
                    unified = True
                    # success, so skip the rest of second, do the next sentence in first:
                    break

                except TE.AcabTypingException as err:
                    # failed, so record the failure,
                    # and try the next sentence in snd
                    failures.append(err)

            # if nothing has managed to unify with fst, error.
            if not unified:
                raise TE.AcabUnifyGroupException(first, second, ctx=ctx, data={'failures': failures})

        return the_ctx
