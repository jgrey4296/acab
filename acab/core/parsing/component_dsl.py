#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
import pyparsing as pp

from acab.interfaces.dsl import DSL_Fragment, DSL_Spec, DSL_Handler
from acab.core.parsing import parsers as PU
from acab.error.parse_exception import AcabParseException

logging = root_logger.getLogger(__name__)

Component_DSL = DSL_Fragment(specs=[DSL_Spec("word.value", struct=PU.HOTLOAD_VALUES, flags=[DSL_Spec.flag_e.COLLECT])],
                             handlers=[DSL_Handler("operators"       , PU.OPERATOR_SUGAR),
                                       DSL_Handler("word.valbind"    , PU.VALBIND),
                                       DSL_Handler("tag.sentence"    , PU.tagSen),
                                       DSL_Handler("operators.modal" , PU.MODAL)])
