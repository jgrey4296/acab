#!/usr/bin/env python
""" A Semantics Class for defining how to print out
ACAB structures
"""
import logging as root_logger

logging = root_logger.getLogger(__name__).root

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from collections import defaultdict
from enum import Enum

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.acab_struct import AcabStruct

from acab.abstract.interfaces import semantic_interfaces as SI

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.abstract.config.config import AcabConfig

from . import util as SemUtil

config = AcabConfig.Get()

OBVIOUS_TYPES = config.value("Print.Data", "SUPPRESSION_TYPES").split(" ")

# TODO replace this with pulling the dict straight from config
PRINT_SENTINEL_JOIN_P = config.prepare(
    "Print.Patterns", "PRINT_SENTINEL_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE]
)

