import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.value.sentence import Sentence
import acab.core.value.default_structure as DS
from acab.core.value.value import AcabValue
from acab.modules.context.context_instance import ContextInstance, MutableContextInstance
from . import exceptions as TE

logging = logmod.getLogger(__name__)

config = AcabConfig()

ROOT_S          = config.prepare("Data", "ROOT")()

BIND_S          = DS.BIND
TYPE_INSTANCE_S = DS.TYPE_INSTANCE
OPERATOR_S      = DS.OPERATOR


TYPE_DEF_S      = config.prepare("Typing.Primitives", "TYPE_DEF")()
OP_DEF_S        = config.prepare("Typing.Primitives", "OP_DEF")()
SUM_DEF_S       = config.prepare("Typing.Primitives", "SUM_DEF")()
STRUCT_S        = config.prepare("Typing.Primitives", "STRUCT")()
TVAR_S          = config.prepare("Typing.Primitives", "TVAR")()
SYNTAX_BIND_S   = config.prepare("Typing.Primitives", "SYNTAX_BIND")()


# TODO make these registrations
TYPE_DEFINITION     = Sentence([TYPE_DEF_S])
SUM_DEFINITION      = Sentence([SUM_DEF_S])
OPERATOR_DEFINITION = Sentence([OP_DEF_S])
# TODO TYPE CLASS

