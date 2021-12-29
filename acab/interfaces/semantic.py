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
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)
from acab import types as AT
from acab.core.data.default_structure import QUERY
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.handler_system import (Handler, Handler_Fragment,
                                            HandlerComponent_i, HandlerSpec,
                                            HandlerSystem_i)
from acab.interfaces.value import Sentence_i, Value_i
from acab.interfaces.util import AcabReducible

Value                = AT.Value
Sentence             = AT.Sentence
Instruction          = AT.Instruction
Structure            = AT.DataStructure
Node                 = AT.Node
Engine               = AT.Engine
CtxSet               = AT.CtxSet
CtxIns               = AT.CtxIns
Handler              = AT.Handler
ProductionOperator   = AT.Operator
ModuleComponents     = AT.ModuleComponents
StructureSemantics   = AT.StructureSemantics
ValueSemantics       = AT.ValueSemantics
StatementSemantics   = AT.StatementSemantics
AbsDepSemantics      = Union[StatementSemantics, StructureSemantics]
SemanticSystem       = AT.SemanticSystem


#----------------------------------------
@dataclass
class SemanticSystem_i(HandlerSystem_i, AcabReducible):
    """
    Map Instructions to Instruction/Structure Semantics
    """
    # TODO possibly re-add hooks / failure handling
    # TODO add a system specific logging handler
    ctx_set         : CtxSet           = field(default=None)

    _operator_cache : Optional[CtxIns] = field(init=False, default=None)

    def build_ctxset(self, ops:List[ModuleComponents]=None):
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
        ctxset.delay(ctxset.delayed_e.DEACTIVATE, ctxset[0].uuid)

        return ctxset

    @property
    def has_op_cache(self) -> bool:
        return self._operator_cache is not None

    @abc.abstractmethod
    def __call__(self, *instructions:Tuple[Instruction], ctxs=None, data=None) -> CtxSet:
        pass

    def extend(self, mods:List[ModuleComponents]):
        logging.info("Extending Semantics")
        semantics = [y for x in mods for y in x.semantics]
        assert(all([isinstance(x, Semantic_Fragment) for x in semantics]))
        for sem_fragment in semantics:
            assert(issubclass(sem_fragment.target_i, SemanticSystem_i)), breakpoint()
            for val in sem_fragment:
                self.register(val)


@dataclass
class StructureSemantics_i(HandlerComponent_i, SemanticSystem_i):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to value semantics
    """

    def __repr__(self):
        return f"{self.__class__.__name__}"

    def __call__(self, sen, struct, ctxs=None, data=None) -> CtxSet:
        assert(isinstance(sen, Sentence_i))
        if QUERY in sen[-1].data and bool(sen[-1].data[QUERY]):
            return self.query(sen, struct, ctxs=ctxs, data=data)

        return self.insert(sen, struct, ctxs=ctxs, data=data)

    def verify(self, instruction:Instruction, data=None, ctxs=None):
        raise NotImplementedError()

    @abc.abstractmethod
    def insert(self, struct:Structure, sen:Sentence, data):
        pass

    @abc.abstractmethod
    def query(self, struct:Structure, sen:Sentence, data):
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
    def make(self, val:Value, data:Dict[Any,Any]=None) -> Node:
        """ Take a value, and return a node, which has been up'd """
        pass
    @abc.abstractmethod
    def up(self, node:Node, data=None) -> Node:
        """ Take ANY node, and add what is needed
        to use for this semantics """
        pass

    def down(self, node:Node, data=None) -> Value:
        return node.value

    @abc.abstractmethod
    def access(self, node:Node, term:Value, data:Dict[Any,Any]=None) -> List[Node]:
        """ Can node A reach the given term """
        pass

    @abc.abstractmethod
    def insert(self, node:Node, new_node:Node, data:Dict[Any,Any]=None) -> Node:
        pass

    @abc.abstractmethod
    def remove(self, node:Node, term:Value, data:Dict[Any,Any]=None) -> Node:
        pass


    def update(self, node:Node, term:Value, data:Dict[Any, Any]=None):
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
    def __call__(self, instruction, semSys, ctxs=None, data=None) -> CtxSet:
        pass


#--------------------------------------------------
@dataclass
class Semantic_Fragment(Handler_Fragment):
    """ Dataclass of Semantic Handlers to be added to the system, and any
    data they require
    """
    target_i : HandlerSystem_i = field(default=SemanticSystem_i)
