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
import abc
from dataclasses import dataclass, field, InitVar

from acab.abstract.config.config import AcabConfig
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.error.acab_print_exception import AcabPrintException
from acab.modules.semantics.context_container import ContextContainer
from acab.abstract.interfaces.value_interfaces import ValueInterface
import acab.abstract.interfaces.util as SU


Node            = 'AcabNode'
Sentence        = 'Sentence'
Printable       = 'Printable'
Value           = 'AcabValue'
Structure       = 'AcabStruct'
Engine          = 'Engine'
Contexts        = 'Contexts'
Handler         = 'SemanticHandler'
AbsDepSemantics = Union['AbstractionSemantics', 'DependentSemantics']
InDepSemantics  = 'IndependentSemantics'

SemanticRetrievedPair   = Tuple[AbsDepSemantics, Structure]

# Note: for dependent and indep, you retrieve semantics of a node,
# for *abstractions*, you're getting the semantics of a *sentence*
def default_key(node:Any, data:Dict[Any,Any]=None) -> str:
    return str(node.value)

def default_failure(semantics, struct, instruction, ctxs, data, err):
    logging.warning("Default Failure: {}".format(err))

def example_hook(semSystem, semantics, struct: Structure, instruction: Sentence, ctxs, data=None):
    pass


#--------------------------------------------------
@dataclass
class SemanticSystem(metaclass=abc.ABCMeta):
    """
    Map Instructions to Abstraction/Dependent Semantics
    """

    # If no applicable semantics found, use default
    base        : AbsDepSemantics                            = field()
    base_struct : Structure                                  = field()
    base_hooks  : Tuple[List[Handler], List[Handler]]        = field(default_factory=tuple)
    # str/iden -> Semantics
    mapping : Dict[str, AbsDepSemantics]                     = field(default_factory=dict)
    structs : Dict[str, Structure]                           = field(default_factory=dict)
    hooks   : Dict[str, Tuple[List[Handler], List[Handler]]] = field(default_factory=dict)
    # sentence -> iden func to determine appropriate semantics
    key     : Callable[[Any, Dict[Any,Any]], str]            = field(default=default_key)
    #
    failure : Callable                                       = field(default=default_failure)

    def __post_init__(self):
        # TODO init any semantics or structs passed in as Class's

        # TODO check depsem -> struct compabilities
        # by running dependent.compatible(struct)

        pass

    def _run_entry_hooks(self, semantics, struct, instruction, ctxs, data):
        the_key = self.key(instruction, data=data)
        if the_key not in self.hooks:
            return

        hooks = self.hooks[the_key][0]
        for hook in hooks:
            hook(self, semantics, struct, instruction, ctxs, data)

    def _run_exit_hooks(self, semantics, struct, instruction, ctxs, data):
        the_key = self.key(instruction, data=data)
        if the_key not in self.hooks:
            return

        hooks = self.hooks[the_key][1]
        for hook in hooks:
            hook(self, semantics, struct, instruction, ctxs, data)

    @abc.abstractmethod
    def retrieve(self, target: Sentence, data=None, override=None) -> SemanticRetrievedPair:
        pass

    @abc.abstractmethod
    def __call__(self, instruction, data=None, override=None, ctxs=None) -> Contexts:
        pass


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

    @abc.abstractmethod
    def compatible(self, struct: Structure) -> bool:
        """ Called to check the semantics can handle the suggested struct """
        pass


class IndependentSemantics(metaclass=abc.ABCMeta):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """
    @abc.abstractmethod
    def make(self, val: Value, data:Dict[Any,Any]=None) -> Node:
        """ Take a value, and return a node, which has been up'd """
        pass
    @abc.abstractmethod
    def up(self, node: Node, data=None) -> Node:
        """ Take ANY node, and add what is needed
        to use for this semantics """
        pass

    def down(self, node: Node, data=None) -> Value:
        return node.value

    @abc.abstractmethod
    def access(self, node: Node, term: Value, data:Dict[Any,Any]=None) -> List[Node]:
        """ Can node A reach the given term """
        pass

    @abc.abstractmethod
    def insert(self, node: Node, new_node: Node, data:Dict[Any,Any]=None) -> Node:
        pass

    @abc.abstractmethod
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
    def __call__(self, instruction, ctxCon, semSys, data=None):
        pass


#--------------------------------------------------
@dataclass
class PrintSemanticSystem(metaclass=abc.ABCMeta):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """

    # Handlers: fn val -> [Val|str]
    handlers        : InitVar[List[Callable]]    = field()
    # Ordered Sieve for lookup of handlers. Most -> least specific
    # fn x -> str
    sieve           : List[Callable]             = field(default_factory=list)
    settings        : Dict[str, str]             = field(default_factory=dict)

    _config    : AcabConfig                      = field(init=False, default_factory=AcabConfig.Get)
    registered_handlers : Dict[str, Callable]    = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[List[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x: x.override if isinstance(x, PrintSemanticSystem.PrintOverride) else None,
        # symbol         : m -> m : any
        lambda x: "_:SYMBOL" if isinstance(x, tuple) else None,
        # enum
        lambda x: "_:SYMBOL" if isinstance(x, enum) else None,
        # exact type     : 1 -> 1 : any / leaf
        lambda x: str(x.type),
        # gen type       : m -> 1 : any / leaf
        # structure      : m -> m : leaf
        # container      : m -> m : leaf
        # component      : m -> m : leaf
        # sentence       : m -> 1 : any / leaf
        lambda x: "_:SENTENCE" if isinstance(x, VI.SentenceInterface) else None,
        # value          : m -> 1 : any
        lambda x: "_:ATOM" if isinstance(x, VI.ValueInterface) else None
    ]


    @dataclass
    class PrintOverride:
        """ Simple Wrapped for forced semantic use """
        override : str              = field()
        data     : 'ValueInterface' = field()
    #----------------------------------------


    def __post_init__(self, handlers):
        # use default sieve if sieve is empty
        if not bool(self.sieve):
            self.sieve += PrintSemanticSystem._default_sieve
        # register provided handlers
        for handler in handlers:
            self._register_handler(handler)

    def _register_handler(self, handler):
        # TODO maybe handle tuples later
        pair_str = handler.paired_with
        assert(pair_str not in self.registered_handlers)
        self.registered_handlers[pair_str] = handler


    def lookup(self, value: ValueInterface) -> 'PrintSemantics':
        # sieve from most to least specific

        for sieve_fn in self.sieve:
            key = sieve_fn(value)
            if bool(key) and key in self.registered_handlers:
                return self.registered_handlers[key]

        # Final resort
        return lambda x: str(x)

    def check(self, val) -> Optional[str]:
        """ Check a value to toggle variations/get defaults"""
        if val in self.settings:
            return self.settings

        return None

    def override(new_target: str, value) -> 'PrintOverride':
        if new_target not in self._registered_handlers:
            raise AcabPrintException(f"Undefined override handler: {new_target}")

        return PrintSemanticSystem.PrintOverride(new_target, value)


    def pprint(self, *args) -> str:
        # TODO add default join symbol
        remaining = list(args[:])
        result = ""
        while bool(remaining):
            current = remaining.pop(0)
            # TODO handle specs that can be transformed to strings
            if isinstance(current, str):
                result += current
            else:
                assert(isinstance(current, ValueInterface))
                #handle
                handler = self.lookup(current)
                if isinstance(handler, PrintSemantics):
                    handled = handler(current, top=self)
                else:
                    handled = handler(current)

                # Add the results of a handler to the head
                if isinstance(handled, list):
                    remaining = handled + remaining
                else:
                    remaining = [handled] + remaining


        return result


    def __call__(self, *args) -> str:
        return self.pprint(*args)
@dataclass
class PrintSemantics(metaclass=abc.ABCMeta):

    paired_with : Sentence       = field()
    transforms  : List[Callable] = field(init=False, default_factory=list)

    def __post_init__(self):
        self.transforms += self.add_str_transforms()

    def add_transforms(self) -> List[Callable]:
        """ Override to add custom transforms in a class """
        return []

    def run_transforms(self, value: ValueInterface, curr_str: List[Any]) -> List[Any]:
        curr = curr_str
        for trans in self.transforms:
            curr = trans(self, value, curr)

        return curr

    @abc.abstractmethod
    def __call__(self, to_print: ValueInterface, top:'PrintSemanticSystem'=None) -> List[Tuple[str,ValueInterface]]:
        pass
