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
import pyparsing as pp
from acab import AcabConfig
from acab.core.config.config import AcabConfig, ConfigSpec
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.printing import default_symbols as DSYM
from acab.core.printing import wrappers as PW
from acab.core.semantics.basic import Semantic_Fragment
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.printing import PrintSemantics_i

from . import parser as SVP
from . import semantics as SVS
from .printer import SenValPrinter, SenValueAwareSentencePrinter

logging = logmod.getLogger(__name__)

config = AcabConfig()

WALK_SEM_HINT    = Sentence() << config.attr.Semantic.Signals.WALK

# TODO sen value spec
Sen_Val_Frag = Semantic_Fragment(specs=[], handlers=[SVS.SenQuerySemantics().as_handler(signal=WALK_SEM_HINT)])

Sen_Val_Parser = ppDSL.DSL_Fragment(specs=[ppDSL.PyParse_Spec("sentence", struct=SVP.HOTLOAD_SENTENCE)],
                                   handlers=[ppDSL.PyParse_Handler("word.value", func=SVP.sen_value)])
