import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.interfaces.value import ValueFactory
import acab.core.defaults.value_keys as DS
from acab.modules.context.context_instance import ContextInstance, MutableContextInstance
from . import exceptions as TE

logging = logmod.getLogger(__name__)

config = AcabConfig()

ROOT_S          = config.attr.Data.ROOT

BIND_S          = DS.BIND
TYPE_INSTANCE_S = DS.TYPE_INSTANCE
OPERATOR_S      = DS.OPERATOR


TYPE_DEF_S      = config.attr.Typing.Primitives.TYPE_DEF
OP_DEF_S        = config.attr.Typing.Primitives.OP_DEF
SUM_DEF_S       = config.attr.Typing.Primitives.SUM_DEF
STRUCT_S        = config.attr.Typing.Primitives.STRUCT
TVAR_S          = config.attr.Typing.Primitives.TVAR
SYNTAX_BIND_S   = config.attr.Typing.Primitives.SYNTAX_BIND
TYPE_CLASS_S    = config.attr.Typing.Primitives.TYPE_CLASS


# TODO make these registrations
TYPE_DEFINITION     = ValueFactory.sen() << TYPE_DEF_S
SUM_DEFINITION      = ValueFactory.sen() << SUM_DEF_S
OPERATOR_DEFINITION = ValueFactory.sen() << OP_DEF_S
TYPE_CLASS          = ValueFactory.sen() << TYPE_CLASS_S
# TODO TYPE CLASS

