"""
Semantics:
Subdivides into *Complete Systems*, *Incomplete Mixins* and *Handlers*

All semantic systems should be able to lift basic sentences up to their preferred internal data format.
And reduce those internal formats back down to basic sentences.

SemanticMap also provide the ability to map a value or node to particular semantics,
and specifies *how* to search for the correct mapping.

Meanwhile ValueSemantics_i are concerned only with the values and structures they have control over.

*Dependent* Semantics factor in contexts and a reference to the engine.


"""

import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Protocol, TypeAlias,
                    Tuple, TypeVar, cast, Type)

logging = root_logger.getLogger(__name__)
from acab import types as AT
from acab.core.data.default_structure import QUERY
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.handler_system import (Handler, Handler_Fragment,
                                            HandlerComponent_i, HandlerSpec,
                                            HandlerSystem_i, _HandlerSystem_d)
from acab.interfaces.value import Sentence_i, Value_i
from acab.interfaces.util import AcabReducible

Value              : TypeAlias = AT.Value
Sentence           : TypeAlias = AT.Sentence
Instruction        : TypeAlias = AT.Instruction
Structure          : TypeAlias = AT.DataStructure
Node               : TypeAlias = AT.Node
Engine             : TypeAlias = AT.Engine
CtxSet             : TypeAlias = AT.CtxSet
CtxIns             : TypeAlias = AT.CtxIns
Handler_A          : TypeAlias = AT.Handler
ProductionOperator : TypeAlias = AT.Operator
ModuleComponents   : TypeAlias = AT.ModuleComponents
StructureSemantics : TypeAlias = AT.StructureSemantics
ValueSemantics     : TypeAlias = AT.ValueSemantics
StatementSemantics : TypeAlias = AT.StatementSemantics
AbsDepSemantics    : TypeAlias = StatementSemantics | StructureSemantics
SemanticSystem     : TypeAlias = AT.SemanticSystem


# Data  #######################################################################
@dataclass
class _SemanticSystem_d(_HandlerSystem_d):
    """
    Map Instructions to Instruction/Structure Semantics
    """
    # TODO possibly re-add hooks / failure handling
    # TODO add a system specific logging handler
    ctx_set         : CtxSet           = field(kw_only=True)

    _operator_cache : None | CtxIns    = field(init=False, default=None)

@dataclass
class Semantic_Fragment(Handler_Fragment):
    """ Dataclass of Semantic Handlers to be added to the system, and any
    data they require
    """
    target_i : None | Type[HandlerSystem_i] = field(default=None)

# Protocols  ##################################################################
class SemanticSystem_i(HandlerSystem_i, AcabReducible, _SemanticSystem_d):

    def build_ctxset(self, ops:list[ModuleComponents]=None):
        """ Build a context set. Use passed in operators if provided.
        Caches operators
        """
        if bool(ops) or self._operator_cache is None:
            ctxset = self.ctx_set.build(ops)
        else:
            ctxset = self.ctx_set.build(self._operator_cache)

        if ctxset._operators is not None:
            self._operator_cache = ctxset._operators.copy()

        # Auto remove the empty context:
        ctxset.delay(ctxset.delayed_e.DEACTIVATE, ctxIns=ctxset[0].uuid)

        return ctxset

    @property
    def has_op_cache(self) -> bool:
        return self._operator_cache is not None

    @abc.abstractmethod
    def __call__(self, *instructions:Iterable[Instruction], ctxs=None, data=None, **kwargs) -> CtxSet:
        pass

    def extend(self, mods:list[ModuleComponents]):
        logging.info("Extending Semantics")
        semantics = [y for x in mods for y in x.semantics]
        assert(all([isinstance(x, Semantic_Fragment) for x in semantics]))
        for sem_fragment in semantics:
            assert(sem_fragment.target_i is None or issubclass(sem_fragment.target_i, SemanticSystem_i))
            for val in sem_fragment:
                self.register(val)


class StructureSemantics_i(HandlerComponent_i, HandlerSystem_i):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to value semantics
    """

    def __repr__(self):
        return f"{self.__class__.__name__}"

    def __call__(self, sen, struct, *, ctxs=None, data=None) -> None | CtxSet:
        assert(isinstance(sen, Sentence_i))
        if QUERY in sen[-1].data and bool(sen[-1].data[QUERY]):
            return self.query(sen, struct, ctxs=ctxs, data=data)

        return self.insert(sen, struct, ctxs=ctxs, data=data)

    def verify(self, instruction:Instruction, *, data=None, ctxs=None):
        raise NotImplementedError()

    @abc.abstractmethod
    def insert(self, sen:Sentence, struct:Structure, *, data, ctxs) -> None:
        pass

    @abc.abstractmethod
    def query(self, sen:Sentence, struct:Structure, *, data, ctxs) -> CtxSet:
        pass

    @abc.abstractmethod
    def compatible(self, struct: Structure) -> bool:
        """ Called to check the semantics can handle the suggested struct """
        pass


class ValueSemantics_i(HandlerComponent_i):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """

    def __repr__(self):
        return f"{self.__class__.__name__}"

    @abc.abstractmethod
    def make(self, val:Value, *, data:dict[Any,Any]=None) -> Node:
        """ Take a value, and return a node, which has been up'd """
        pass
    @abc.abstractmethod
    def up(self, node:Node, *, data=None) -> Node:
        """ Take ANY node, and add what is needed
        to use for this semantics """
        pass

    def down(self, node:Node, *, data=None) -> Value:
        return node.value

    @abc.abstractmethod
    def access(self, node:Node, term:Value, *, data:dict[Any,Any]=None) -> list[Node]:
        """ Can node A reach the given term """
        pass

    @abc.abstractmethod
    def insert(self, node:Node, new_node:Node, *, data:dict[Any,Any]=None) -> Node:
        pass

    @abc.abstractmethod
    def remove(self, node:Node, term:Value, *, data:dict[Any,Any]=None) -> Node:
        pass


    def update(self, node:Node, term:Value, *, data:dict[Any, Any]=None):
        # TODO
        pass

class StatementSemantics_i(HandlerComponent_i):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...

    AbsSems use the total semantic system to call other AbSems, or
    DepSems
    """

    def __repr__(self):
        return f"{self.__class__.__name__}"

    def verify(self, instruction:Instruction):
        pass

    @abc.abstractmethod
    def __call__(self, instruction, semSys, *, ctxs=None, data=None) -> CtxSet:
        pass


#--------------------------------------------------
