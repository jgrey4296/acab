#!/usr/bin/env python3
from dataclasses import dataclass, field
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.value.default_structure as DS
import acab.interfaces.value as VI
from acab.core.config.config import GET, AcabConfig, ConfigSpec
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.printing import basic
from acab.core.printing import default_symbols as DSYM
from acab.core.printing import wrappers as PW
from acab.interfaces.printing import PrintSemantics_i

config = GET()
WALK_SEM_HINT    = Sentence([config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()])

@dataclass
class DFSSenPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    signal : str = field(default=WALK_SEM_HINT)

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, VI.Sentence_i))
        return_list = []
        if value[0].is_at_var:
            return_list.append(top.override(False, value[0], data={"no_modal": True}))
            return_list.append(" ᛦ ")
            return_list += [top.override(False, x, data={"no_modal": True}) for x in value.words[1:]]
        else:
            return_list.append("ᛦ ")
            return_list += [top.override(False, x, data={"no_modal": True}) for x in value.words]


        return return_list
