#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from dataclasses import dataclass, field
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.value.default_structure as DS
import acab.interfaces.value as VI
from acab.core.config.config import AcabConfig
from acab.core.printing import default_symbols as DSYM
from acab.core.printing import wrappers as PW
from acab.interfaces.printing import PrintSemantics_i

config = AcabConfig()

@dataclass
class SenValPrinter(PrintSemantics_i):

    signal : str = field(default="SEN_VAL")

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, VI.Sentence_i))
        return_list = []
        return_list.append("[[")
        return_list.append(value)
        return_list.append("]]")
        return_list.append(top.override("MODAL", value, data=data))

        return return_list

@dataclass
class SenValueAwareSentencePrinter(PrintSemantics_i):

    signal : str = field(default="SENTENCE")

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, VI.Sentence_i))
        return_list = []

        if DS.NEGATION in value.data and value.data[DS.NEGATION]:
            return_list.append(DSYM.NEGATION_SYM)

        for word in value.words[:-1]:
            if isinstance(word, VI.Sentence_i):
                return_list.append(top.override("SEN_VAL", word))
            else:
                return_list.append(word)


        if isinstance(value.words[-1], VI.Sentence_i):
            return_list.append(top.override("SEN_VAL", value.words[-1], data={"no_modal": True}))
        elif not isinstance(value.words[-1], AcabStatement):
            return_list.append(PW._suppress_modal(top, value.words[-1]))
        else:
            return_list.append(value.words[-1])

        return return_list
