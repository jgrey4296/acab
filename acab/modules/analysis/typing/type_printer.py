#!/usr/bin/env python3
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from itertools import zip_longest, filterfalse, starmap

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
from acab.core.config.config import GET, AcabConfig, ConfigSpec
from acab.core.data.values import Sentence, AcabStatement
from acab.interfaces.printing import PrintSemantics_i
from acab.core.printing import default_symbols as DSYM
from acab.core.printing import wrappers as PW


config = GET()

@dataclass
class TypeAwareValuePrinter(PrintSemantics_i):

    signal : str = field(default="_:TYPE_INSTANCE")

    def __call__(self, value, top=None, data=None):
        return_list = []
        type_str = str(value.data[DS.TYPE_INSTANCE])
        if type_str == "_:ATOM":
            return []

        return_list.append("::")
        return_list.append(value.data[DS.TYPE_INSTANCE])

        return return_list
