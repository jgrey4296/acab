#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as logmod
import pyparsing as pp

from acab import GET
from acab.core.parsing import parsers as PU
from acab.error.parse import AcabParseException
from acab.core.parsing import pyparse_dsl as ppDSL
logging = logmod.getLogger(__name__)

config = GET()

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

Component_DSL = DSL_Fragment(specs=[DSL_Spec("word.value", struct=PU.HOTLOAD_VALUES, flags=[DSL_Spec.flag_e.COLLECT])],
                             handlers=[DSL_Handler("operators"       , PU.OPERATOR_SUGAR),
                                       DSL_Handler("word.valbind"    , PU.VALBIND),
                                       DSL_Handler("tag.sentence"    , PU.tagSen),
                                       DSL_Handler("operators.modal" , PU.MODAL)])
