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

import acab.core.defaults.value_keys as DS
import acab.interfaces.context as CtxInt
import acab.interfaces.data as DI
import acab.interfaces.value as VI
from acab import AcabConfig
from acab import types as AT
from acab.core.util.delayed_commands import DelayedCommands_i
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import ProductionContainer
from acab.error.context import AcabContextException
from acab.interfaces.value import Sentence_i
from acab.modules.context.constraints import ConstraintCollection
config = AcabConfig()

CONSTRAINT_S     = DS.CONSTRAINT
NEGATION_S       = DS.NEGATION

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = AT.Value
Statement        = AT.Instruction
Sen              = Sentence_i
Node             = AT.StructView
ModuleFragment   = AT.ModuleFragment
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
                logging.debug("Got an ctxinst.__getitem__ an @var")
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

    @property
    def current_node(self):
        return self._current
    def copy(self, mask=None, **kwargs):
        logging.debug("Copied Ctx Instance")
        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()
        if 'nodes' not in kwargs:
            kwargs['nodes'] = self.nodes.copy()

        if mask is not None:
            kwargs['data'] = {x:y for x,y in kwargs['data'].items() if x not in mask}

        copied = replace(self,
                         uuid=uuid1(),
                         data=kwargs['data'],
                         nodes=kwargs['nodes'],
                         _parent_ctx=self)

        assert(self.uuid != copied.uuid)
        assert(id(self.data) != id(copied.data))
        assert(self.uuid in copied._lineage)
        return copied

    def progress(self, word, nodes, sub_binds=None) -> list[CtxIns]:
        """
        Create either:
        a single new ctxinst, with multiple new bindings, or
        multiple new ctxinsts, with a single new binding in each

        sub_binds specifies sub-structural binds to make.
        ie: binding a value to $x, and its type to $y
        """
        match word, nodes:
            case dict(), dict():
                return self._dict_progress(word, nodes)
            case VI.Value_i(), list():
                return self._val_progress(word, nodes, sub_binds=sub_binds)
            case _, _:
                raise TypeError(f"Unexpected type for ContextInstance.Progress: {type(word)}, {type(nodes)}")

    def _val_progress(self, word, nodes, sub_binds=None):
        """
        Create multiple new ctx instances, one for each node
        """
        sub_binds = sub_binds or []
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

            for key, bind in sub_binds:
                assert(bind.is_var)
                if bind[0].key() in ctxInst:
                    bind_val = ctxInst[bind[0].key()]
                else:
                    bind_val = None

                if key in node.value.data:
                    val = node.value.data[key]
                elif hasattr(node.value, key):
                    val = getattr(node.value, key)
                else:
                    raise AcabContextException("Bad SubBind, no value to bind", context=(key, bind))

                if bind_val and bind_val != val:
                    raise AcabContextException("Subbind doesn't match", context=(key, bind_val, val))
                elif not bind_val:
                    ctxInst.data[bind[0].key()] = val

        # Then return the new ctxinsts
        return [x[0] for x in extensions]

    def _dict_progress(self, val_binds:dict[str, Any]=None, node_binds:dict[str,Node]=None) -> [CtxIns]:
        """
        bind multiple values simulatenously, creating a single new ctxInstance
        """
        data_copy   = self.data.copy()
        nodes_copy  = self.nodes.copy()

        data_copy  |= (val_binds or {})
        nodes_copy |= (node_binds or {})

        return [self.copy(data=data_copy, nodes=nodes_copy)]

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

    parent       : None|CtxSet    = field()
    base         : CtxIns         = field()
    data         : dict[Any, Any] = field(default_factory=dict)
    uuid         : UUID           = field(default_factory=uuid1)
    exact        : bool           = field(default=False)

    _finished    : None | CtxIns    = field(init=False, default=None)

    class EarlyExitException(BaseException):
        pass

    def __contains__(self, value: int|str|Value):
        match value:
            case VI.Sentence_i():
                key = str(value)
            case VI.Value_i():
                key = value.key()
            case _:
                key = value

        direct   = str(key) in self.data
        indirect = value in self.base
        return direct or indirect

    def __getitem__(self, value: Value):
        if self.base.exact and value not in self:
            raise AcabContextException("Not Found in Context", context=value)

        match value:
            case VI.Sentence_i():
                key = str(value)
            case VI.Value_i():
                key = value.key()
            case _:
                key = value

        if str(key) in self.data:
            return self.data[str(key)]

        return self.base[value]

    def __setitem__(self, key: Value, value: Value):
        match key:
            case VI.Sentence_i():
                key = str(key)
            case VI.Value_i():
                key = key.key()
            case _:
                pass

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
            # as everything went fine
            return True

        # Do complain about anything else
        return False

    def __len__(self):
        return len(self.data) + len(self.base)

    def finish(self):
        self._finished = self.base.progress(self.data, {})[0]
        if self.parent is not None:
            self.parent.push(self._finished)

    @property
    def final_ctx(self):
        assert(self._finished is not None)
        return self._finished

    def progress(self, words, nodes, sub_binds=None) -> list[CtxIns]:
        """
        Create either:
        a single new ctxinst, with multiple new bindings, or
        multiple new ctxinsts, with a single new binding in each

        """
        match words, nodes:
            case dict(), dict():
                self.data |= (words or {})
            case _, _:
                raise TypeError(f"Unexpected type for ContextInstance.Progress: {type(word)}, {type(nodes)}")

        # assert(not any([x in self.data for x in the_dict])), breakpoint()
        # assert(not any([x in self.base for x in the_dict])), breakpoint()

        return self

    def bind(self, word, nodes):
        raise NotImplementedError()

    def bind_dict(self, the_dict):
        raise DeprecationWarning()

    def to_sentences(self):
        raise NotImplementedError()

    def __iter__(self):
        raise NotImplementedError("Iteration on a MutableContextInstance is nonsensical")

    @property
    def current_node(self):
        self.base.current_node
