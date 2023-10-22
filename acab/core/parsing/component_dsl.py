#!/usr/bin/env python3
##-- imports
from __future__ import annotations
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as logmod
import pyparsing as pp

import acab
from acab.core.parsing import parsers as PU
from acab.error.parse import AcabParseException
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.util.fragments import DSL_Fragment

##-- end imports

logging = logmod.getLogger(__name__)
config  = acab

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

Component_DSL = DSL_Fragment(specs=[DSL_Spec("word.value", struct=PU.HOTLOAD_VALUES, flags=[DSL_Spec.flag_e.COLLECT]),
                                    DSL_Spec("word.annotation.head", struct=PU.HOTLOAD_HEAD_ANNOTATIONS, flags=[DSL_Spec.flag_e.COLLECT]),
                                    DSL_Spec("word.annotation.post", struct=PU.HOTLOAD_POST_ANNOTATIONS, flags=[DSL_Spec.flag_e.COLLECT])],
                             handlers=[DSL_Handler("operators"       , func=PU.OPERATOR_SUGAR),
                                       DSL_Handler("word.valbind"    , func=PU.VALBIND),
                                       DSL_Handler("tag.sentence"    , func=PU.tagSen),
                                       DSL_Handler("operators.modal" , func=PU.MODAL),
                                       ])
