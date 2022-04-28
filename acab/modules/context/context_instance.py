#!/usr/bin/env python3
from __future__ import annotations
import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

import acab.interfaces.context as CtxInt
import acab.interfaces.value as VI
from acab.core.config.config import GET
from acab.core.value.instruction import ProductionComponent, ProductionContainer
from acab.core.util.delayed_commands import DelayedCommands_i
from acab.error.context import AcabContextException
from acab.interfaces.value import Sentence_i
from acab.modules.context.constraints import ConstraintCollection

config = GET()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = 'AcabValue'
Statement        = 'Instruction'
Sen              = Sentence_i
Node             = 'AcabNode'
ModuleComponents = "ModuleComponents"
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")

NamedCtxSet      = CtxInt.NamedCtxSet_d
ContextFailState = CtxInt.ContextFailState_d

@dataclass(frozen=True)
class ContextInstance(CtxInt.ContextInstance_i):

    data              : dict[str, Any]  = field(default_factory=dict)
    nodes             : dict[str, Node] = field(default_factory=dict)
    uuid              : UUID            = field(default_factory=uuid1)
    _parent_ctx       : CtxIns          = field(default=None)
    exact             : bool            = field(default=False)

    _current          : Node            = field(init=False, default=None)
    _depth            : int             = field(init=False, default=0)
    _lineage          : set[UUID]       = field(init=False, default_factory=set)

    def __post_init__(self):
        self._lineage.add(self.uuid)
        if self._parent_ctx is not None:
            object.__setattr__(self, "_depth", self._parent_ctx._depth + 1)
            self._lineage.update(self._parent_ctx._lineage)

    def __hash__(self):
        return hash(self.uuid)

    def __contains__(self, value: int|str|Value):
        key = value
        if isinstance(value, VI.Sentence_i):
            key = str(value)
        elif isinstance(value, VI.Value_i):
            key = value.key()

        return str(key) in self.data

    def __getitem__(self, value: Value):
        # TODO maybe handle AT_BINDs
        key = str(value)
        match value:
            case VI.Value_i() if value.is_at_var:
                logging.warning("Tried to ctxinst.__getitem__ an @var")
                key = value.key()
            case VI.Sentence_i() if value.is_var:
                key = value[0].key()
            case VI.Sentence_i():
                key = str(value)
            case VI.Value_i():
                key = value.key()
            case _, self.exact:
                raise AcabContextException("Not Found in Context", context=value)

        if key in self.data:
            return self.data[key]

        return value

    def __getattribute__(self, value):
        try:
            return object.__getattribute__(self, value)
        except AttributeError as err:
            if not value in self:
                raise err
            return self.__getitem__(value)

    def __bool__(self):
        return bool(self.data)
    def __len__(self):
        return len(self.data)
    def __iter__(self):
        return iter(self.data.values())

    def __repr__(self):
        binds  = ", ".join([x for x in self.data.keys()])
        return f"(CtxInst: Bindings: {binds})"

    def copy(self, **kwargs):
        logging.debug("Copied Ctx Instance")
        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()
        if 'nodes' not in kwargs:
            kwargs['nodes'] = self.nodes.copy()

        copied = replace(self,
                         uuid=uuid1(),
                         data=kwargs['data'],
                         nodes=kwargs['nodes'],
                         _parent_ctx=self)

        assert(self.uuid != copied.uuid)
        assert(id(self.data) != id(copied.data))
        assert(self.uuid in copied._lineage)
        return copied

    def bind(self, word, nodes, sub_binds=None) -> list[CtxIns]:
        """
        create a binding between word and the nodes provided, generating len(nodes)
        new ctxInstances
        """
        if sub_binds is None:
            sub_binds = []
        # Make len(nodes) new ctxins for the new bindings
        extensions = [(self.copy(), x) for x in nodes]
        # Get the binding name. ie: $x
        word_str = str(word)
        # Now bind
        for ctxInst, node in extensions:
            ctxInst.set_current_node(node)
            if word.is_var:
                ctxInst.data[word_str]  = node.value
                ctxInst.nodes[word_str] = node

            matches = []
            for key, bind in sub_binds:
                if key in node.value.data:
                    val = node.value.data
                elif hasattr(node.value, key):
                    val = getattr(node.value, key)
                else:
                    raise AcabContextException("Bad SubBind, no value to bind", context=(key, bind))

                matches += bind.match(val)

            for x,y in matches:
                if x not in ctxInst.data:
                    ctxInst.data[str(x)] = y

        return [x[0] for x in extensions]

    def bind_dict(self, val_binds:dict[str, Any]=None, node_binds:dict[str,Node]=None) -> CtxIns:
        """
        bind multiple values simulatenously, creating a single new ctxInstance
        """
        data_copy = self.data.copy()
        nodes_copy = self.nodes.copy()

        if val_binds is not None:
            data_copy.update(val_binds)
        if node_binds is not None:
            nodes_copy.update(node_binds)
        return self.copy(data=data_copy, nodes=nodes_copy)

    def set_current_node(self, node):
        object.__setattr__(self, "_current", node)
        return self

    def set_current_binding(self, word):
        if word.key() not in self.nodes:
            raise AcabContextException("No Recognised binding", context=(word, self.nodes))

        self.set_current_node(self.nodes[word.key()])
        return self

    def finish(self):
        return self

@dataclass
class MutableContextInstance(CtxInt.ContextInstance_i):
    """ Wrap A Context Instance with an smart dictionary.
    Changes are inserted into the dictionary, until context is exited
    exit creates a new CtxIns, integrating changes """

    parent       : None|CtxSet = field()
    base         : CtxIns           = field()
    data         : dict[Any, Any]   = field(default_factory=dict)
    uuid         : UUID             = field(default_factory=uuid1)
    exact        : bool             = field(default=False)

    _finished    : None | CtxIns    = field(init=False, default=None)
    class EarlyExitException(BaseException):
        pass

    def __contains__(self, value: int|str|Value):
        key = value
        if isinstance(value, VI.Sentence_i):
            key = str(value)
        elif isinstance(value, VI.Value_i):
            key = value.key()

        direct = str(key) in self.data
        indirect = value in self.base
        return direct or indirect

    def __getitem__(self, value: Value):
        if self.base.exact and value not in self:
            raise AcabContextException("Not Found in Context", context=value)

        key = value
        if isinstance(value, VI.Sentence_i):
            key = str(value)
        elif isinstance(value, VI.Value_i):
            key = value.key()

        if str(key) in self.data:
            return self.data[str(key)]

        return self.base[value]

    def __setitem__(self, key: Value, value: Value):
        if isinstance(key, VI.Sentence_i):
            key = str(key)
        elif isinstance(key, VI.Value_i):
            key = key.key()

        self.data[str(key)] = value


    def __getattribute__(self, value):
        try:
            return object.__getattribute__(self, value)
        except AttributeError as err:
            if not value in self:
                raise err
            return self.__getitem__(value)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        early_exit = exc_type is MutableContextInstance.EarlyExitException
        if early_exit or exc_type is None:
            self.finish()

        if early_exit:
            # Don't complain about early exits
            return True

        # Do complain about anything else
        return False

    def __iter__(self):
        raise NotImplementedError("Iteration on a MutableContextInstance is nonsensical")

    def finish(self):
        self._finished = self.base.bind_dict(self.data)
        if self.parent is not None:
            self.parent.push(self._finished)

    @property
    def final_ctx(self):
        assert(self._finished is not None)
        return self._finished

    def bind(self, word, nodes):
        raise NotImplementedError()

    def bind_dict(self, the_dict):
        # assert(not any([x in self.data for x in the_dict])), breakpoint()
        # assert(not any([x in self.base for x in the_dict])), breakpoint()

        self.data.update(the_dict)
        return self

    def to_sentences(self):
        raise NotImplementedError()

    def __len__(self):
        return len(self.data) + len(self.base)
