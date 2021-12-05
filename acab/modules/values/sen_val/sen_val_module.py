#!/usr/bin/env python3
import logging as root_logger
from dataclasses import dataclass, field
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
import pyparsing as pp

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
from acab.core.config.config import GET, AcabConfig, ConfigSpec
from acab.interfaces.dsl import DSL_Fragment, DSL_Spec, DSL_Handler, DSLBuilder
from acab.interfaces.printing import PrintSemantics_i
from acab.interfaces.semantic import Semantic_Fragment
from acab.core.printing import default_symbols as DSYM
from acab.core.data.values import AcabStatement, Sentence
from acab.core.printing import wrappers as PW

from . import sen_val_parser as SVP

logging = root_logger.getLogger(__name__)

config = GET()
WALK_SEM_HINT    = Sentence.build([config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()])

# TODO sen value spec
Sen_Val_Frag = Semantic_Fragment(specs=[], statement=[SenQuerySemantics().as_handler(WALK_SEM_HINT)])

SenVal_Parser = DSL_Fragment(specs=[DSL_Spec("sentence", SVP.HOTLOAD_SENTENCE)],
                             handlers=[DSL_Handler("word.value", SVP.sen_value)])


@dataclass
class SenValPrinter(PrintSemantics_i):

    signal : str = field(default="_:SEN_VAL")

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, VI.Sentence_i))
        return_list = []
        return_list.append("[[")
        return_list.append(value)
        return_list.append("]]")
        return_list.append(top.override("_:MODAL", value, data=data))

        return return_list

@dataclass
class SenValueAwareSentencePrinter(PrintSemantics_i):

    signal : str = field(default="_:SENTENCE")

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, VI.Sentence_i))
        return_list = []

        if DS.NEGATION in value.data and value.data[DS.NEGATION]:
            return_list.append(DSYM.NEGATION_SYM)

        for word in value.words[:-1]:
            if isinstance(word, Sentence):
                return_list.append(top.override("_:SEN_VAL", word))
            else:
                return_list.append(word)


        if isinstance(value.words[-1], Sentence):
            return_list.append(top.override("_:SEN_VAL", value.words[-1], data={"no_modal": True}))
        elif not isinstance(value.words[-1], AcabStatement):
            return_list.append(PW._suppress_modal(top, value.words[-1]))
        else:
            return_list.append(value.words[-1])

        return return_list
