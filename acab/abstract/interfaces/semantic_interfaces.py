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
SemanticUnion = Union['IndependentSemantics', 'DependentSemantics']

T  = TypeVar('T')
T2 = TypeVar('T2')

SemSearchF = Callable[[T], Optional[SemanticUnion]]
SemSearch = List[SemSearchF]

def default_key(node:Node, data:Dict[Any,Any]=None) -> str:
    return str(node.value)

def default_failure(struct, sen, data, err):
    logging.warning("Default Failure: {}".format(err))

def example_hook(struct: Structure, sen: Sentence, data=None, *args):
    pass



# Components
class AbstractionSemantics(metaclass=abc.ABCMeta):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...
    """

    def verify(self, instruction):
        pass
    def __call__(self, instruction, ctxCon):
        pass


@dataclass
class DependentSemantics(metaclass=abc.ABCMeta):
    """
    Dependent Semantics rely on external context like the engine
    """
    # If no applicable semantics found, use default
    base    : 'IndependentSemantics'               = field()
    # str/iden -> IndependentSemantics
    mapping : Dict[str, 'IndependentSemantics']    = field(default_factory=dict)
    # node -> iden func to determin indep semantics
    key     : Callable[[Node, Dict[Any,Any]], str] = field(default=default_key)
    #
    failure : Callable                             = field(default=default_failure)
    # Dep Sem Entry/Exit Hooks
    hooks   : Tuple[List[Handler], List[Handler]]  = field(default_factory=tuple)
    # Query Behaviour

    def __post_init__(self):
        if not bool(self.hooks):
            self.hooks = ([], [])

    def _run_entry_hooks(self, struct, sen, data):
        for hook in self.hooks[0]:
            hook(struct, sen, data)

    def _run_exit_hooks(self, struct, sen, data):
        for hook in self.hooks[1]:
            hook(struct, sen, data)

    def retrieve(self, node: Node, data=None) -> 'IndependentSemantics':
        lookup_key = self.key(node, data)
        if lookup_key in self.mapping:
            return self.mapping[lookup_key]

        return self.base


    def insert(self, struct: Structure, sen: Sentence, data:Dict[Any,Any]=None) -> Optional[Node]:
        try:
            self._run_entry_hooks(struct, sen, data)
            # perform
            self._insert(struct, sen, data)
        except AcabSemanticException as err:
            self.failure(struct, sen, data, err)
        except Exception as err:
            breakpoint()
            logging.error("Error: {}".format(str(err)))
        finally:
            self._run_exit_hooks(struct, sen, data)

    def query(self, struct: Structure, sen: Sentence, data:Dict[Any,Any]=None, ctxs:Contexts=None) -> Contexts:
        try:
            # If no ctx, create it
            self._run_entry_hooks(struct, sen, data)
            self._query(struct, sen, data, ctxs)
        except AcabSemanticException as err:
            self.failure(struct, sen, data, err)
        except Exception as err:
            breakpoint()
            logging.error("Error: {}".format(str(err)))
        finally:
            self._run_exit_hooks(struct, sen, data)
            return ctxs

    def trigger(self, struct: Structure, sen: Sentence, data:Dict[Any,Any]=None) -> Any:
        try:
            self._run_entry_hooks(struct, sen, data)
            self._trigger(struct, sen, data)
        except AcabSemanticException as err:
            self.failure(struct, sen, data, err)
        except Exception as err:
            breakpoint()
            logging.error("Error: {}".format(str(err)))
        finally:
            self._run_exit_hooks(struct, sen, data)



    @abc.abstractmethod
    def _insert(self, struct, sen, data):
        pass

    @abc.abstractmethod
    def _query(self, struct, sen, data):
        pass

    @abc.abstractmethod
    def _trigger(self, struct, sen, data):
        pass

    def verify(self, instruction):
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

    def equal(self, val1:Node, val2:Node, data:Dict[Any,Any]=None) -> bool:
        pass







