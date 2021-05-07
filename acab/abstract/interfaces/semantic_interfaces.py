"""
Semantics:
Subdivides into *Complete Systems*, *Incomplete Mixins* and *Handlers*

All semantic systems should be able to lift basic sentences up to their preferred internal data format.
And reduce those internal formats back down to basic sentences.

SemanticMap also provide the ability to map a value or node to particular semantics,
and specifies *how* to search for the correct mapping.

Meanwhile IndependentSemantics are concerned only with the values and structures they have control over.

*Dependent* Semantics factor in contexts and a reference to the engine.


"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.abstract.semantics import util as SemUtil
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_container import ContextContainer

import abc
from dataclasses import dataclass, field

Node          = 'AcabNode'
Sentence      = 'Sentence'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'
Handler       = 'SemanticHandler'
AbsDepSemantics = Tuple['AbstractionSemantics', 'DependentSemantics']
InDepSemantics = 'IndependentSemantics'

T  = TypeVar('T')

def default_key(node:Node, data:Dict[Any,Any]=None) -> str:
    return str(node.value)

def default_failure(semantics, struct, instruction, ctxs, data, err):
    logging.warning("Default Failure: {}".format(err))

def example_hook(semantics, struct: Structure, instruction: Sentence, ctxs, data=None):
    pass





@dataclass
class SemanticSystem(metaclass=abc.ABCMeta):
    """
    Map Instructions to Abstraction/Dependent Semantics
    """

    # If no applicable semantics found, use default
    base    : AbsDepSemantics                          = field()
    # str/iden -> Semantics
    mapping : Dict[str, AbsDepSemantics]               = field(default_factory=dict)
    structs : Dict[str, Structure]                     = field(default_factory=dict)
    # sentence -> iden func to determine appropriate semantics
    key     : Callable[[T, Dict[Any,Any]], str]        = field(default=default_key)
    #
    failure : Callable                                 = field(default=default_failure)
    # Dep Sem Entry/Exit Hooks
    hooks   : Tuple[List[Handler], List[Handler]]      = field(default_factory=tuple)

    def __post_init__(self):
        if not bool(self.hooks):
            self.hooks = ([], [])

    def _run_entry_hooks(self, semantics, struct, instruction, ctxs, data):
        for hook in self.hooks[0]:
            hook(semantics, struct, instruction, ctxs, data)

    def _run_exit_hooks(self, semantics, struct, instruction, ctxs, data):
        for hook in self.hooks[1]:
            hook(semantics, struct, instruction, ctxs, data)


    def retrieve(self, target: Sentence, data=None, override=None):
        lookup_key = self.key(target, data=data)
        if override is not None:
            # TODO
            lookup_key = override
        semantics  = self.base
        struct     = None
        if lookup_key in self.mapping:
            semantics = self.mapping[lookup_key]
        if lookup_key in self.structs:
            struct = self.structs[lookup_key]

        return (semantics, struct)

    def __call__(self, instruction, data=None, override=None, ctxs=None):
        """ Perform an instruction by mapping it to a semantics """
        result = None
        if ctxs is None:
            # TODO: finish this
            ctxs = ContextContainer.build()
        try:
            semantics, struct = self.retrieve(instruction, data=data, override=override)
            self._run_entry_hooks(semantics, struct, instruction, ctxs, data)
            # run the semantics
            if isinstance(semantics, AbstractionSemantics):
                assert(struct is None)
                semantics(instruction, ctxs, self, data=data)
            else:
                assert(struct is not None)
                semantics(struct, instruction, data=data, ctxs=ctxs)
        except AcabSemanticException as err:
            self.failure(semantics, struct, instruction, ctxs, data, err)
        finally:
            self._run_exit_hooks(semantics, struct, instruction, ctxs, data)
            return ctxs

@dataclass
class DependentSemantics(metaclass=abc.ABCMeta):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to independent semantics
    """

    # If no applicable semantics found, use default
    base    : InDepSemantics                       = field()
    # str/iden -> Semantics
    mapping : Dict[str, InDepSemantics]            = field(default_factory=dict)
    # node -> iden func to determine appropriate semantics
    key     : Callable[[Node, Dict[Any,Any]], str] = field(default=default_key)

    def retrieve(self, target: None, data=None):
        lookup_key = self.key(target, data)
        semantics = self.base
        if lookup_key in self.mapping:
            semantics = self.mapping[lookup_key]

        return self.base


    def to_sentences(self, struct, data=None, ctxs=None):
        """ Reduce a struct down to sentences, for printing """
        raise NotImplementedError()

    def verify(self, instruction, data=None, ctxs=None):
        raise NotImplementedError()
    @abc.abstractmethod
    def insert(self, struct, sen, data):
        pass

    @abc.abstractmethod
    def query(self, struct, sen, data):
        pass

class IndependentSemantics(metaclass=abc.ABCMeta):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """
    def make(self, val: Value, data:Dict[Any,Any]=None) -> Node:
        """ Take a value, and return a node, which has been up'd """
        pass

    def up(self, node: Node, data=None) -> Node:
        """ Take ANY node, and add what is needed
        to use for this semantics """
        pass

    def down(self, node: Node, data=None) -> Value:
        return node.value

    def access(self, node: Node, term: Value, data:Dict[Any,Any]=None) -> List[Node]:
        """ Can node A reach the given term """
        pass

    def insert(self, node: Node, new_node: Node, data:Dict[Any,Any]=None) -> Node:
        pass

    def remove(self, node: Node, term: Value, data:Dict[Any,Any]=None) -> Optional[Node]:
        pass


class AbstractionSemantics(metaclass=abc.ABCMeta):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...

    AbsSems use the total semantic system to call other AbSems, or
    DepSems
    """

    def verify(self, instruction):
        pass
    @abc.abstractmethod
    def __call__(self, instruction, ctxCon, semMap, data=None):
        pass
