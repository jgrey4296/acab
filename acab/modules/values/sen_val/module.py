#!/usr/bin/env python3
##-- imports
from __future__ import annotations

import logging as logmod
from dataclasses import dataclass, field
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
import pyparsing as pp
import acab
from acab.core.defaults import print_symbols as DSYM
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.printing import wrappers as PW
from acab.core.util.fragments import DSL_Fragment, Semantic_Fragment
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.printing import PrintSemantics_i

from . import parser as SVP
from . import semantics as SVS
from .printer import SenValPrinter, SenValueAwareSentencePrinter

##-- end imports

logging = logmod.getLogger(__name__)

config = acab.config

WALK_SEM_HINT    = Sentence() << config.any_of().semantic.signals.WALK

# TODO sen value spec
Sen_Val_Frag = Semantic_Fragment(handlers=[SVS.SenQuerySemantics().as_handler(signal=WALK_SEM_HINT)])

Sen_Val_Parser = DSL_Fragment(specs=[ppDSL.PyParse_Spec("sentence", struct=SVP.HOTLOAD_SENTENCE)],
                              handlers=[ppDSL.PyParse_Handler("word.value"               , func=SVP.sen_value),
                                        ppDSL.PyParse_Handler("word.annotation"          , func=SVP.flatten_annotation),
                                        ppDSL.PyParse_Handler("word.annotation.head"     , func=SVP.flatten_annotation),
                                        ppDSL.PyParse_Handler("word.annotation.post"     , func=SVP.flatten_annotation),
                                        ppDSL.PyParse_Handler("sentence.annotation.head" , func=SVP.flatten_annotation),
                                        ppDSL.PyParse_Handler("sentence.annotation.post" , func=SVP.flatten_annotation),
                                        ])
