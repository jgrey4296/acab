#!/usr/bin/env python3
import logging as logmod
from dataclasses import dataclass, field
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
import pyparsing as pp
from acab.core.config.config import GET, AcabConfig, ConfigSpec
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.printing import default_symbols as DSYM
from acab.core.printing import wrappers as PW
from acab.interfaces.printing import PrintSemantics_i
from acab.interfaces.semantic import Semantic_Fragment

from . import parser as SVP
from . import semantics as SVS

logging = logmod.getLogger(__name__)

config = GET()
WALK_SEM_HINT    = Sentence([config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()])

# TODO sen value spec
Sen_Val_Frag = Semantic_Fragment(specs=[], handlers=[SVS.SenQuerySemantics().as_handler(signal=WALK_SEM_HINT)])

SenVal_Parser = ppDSL.DSL_Fragment(specs=[ppDSL.PyParse_Spec("sentence", struct=SVP.HOTLOAD_SENTENCE)],
                                   handlers=[ppDSL.PyParse_Handler("word.value", SVP.sen_value)])


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
            if isinstance(word, Sentence):
                return_list.append(top.override("SEN_VAL", word))
            else:
                return_list.append(word)


        if isinstance(value.words[-1], Sentence):
            return_list.append(top.override("SEN_VAL", value.words[-1], data={"no_modal": True}))
        elif not isinstance(value.words[-1], AcabStatement):
            return_list.append(PW._suppress_modal(top, value.words[-1]))
        else:
            return_list.append(value.words[-1])

        return return_list
