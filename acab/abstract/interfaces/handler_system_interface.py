#!/opts/anaconda3/envs/ENV/python
import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.error.acab_base_exception import AcabBaseException

logging = root_logger.getLogger(__name__)

Structure      = 'AcabStruct'
Handler        = 'HandlerComponent'
ValueInterface = 'ValueInterface'
Sentence       = 'Sentence'
HandlerTuple   = Tuple[Handler, Optional[Structure]]
Overrider      = 'HandlerOverride'

@dataclass
class HandlerSystemInterface(metaclass=abc.ABCMeta):

    handlers : InitVar[List[Callable]]    = field()
    structs  : InitVar[List[Structure]]   = field()
    default  : Tuple[Callable, Structure] = field(default=(None,None))
    sieve    : List[Callable]             = field(default_factory=list)

    registered_handlers : Dict[str, Callable]  = field(init=False, default_factory=dict)
    registered_structs  : Dict[str, Structure] = field(init=False, default_factory=dict)

    # TODO Make this a classvar
    _default_sieve : ClassVar[List[Callable]] = []

    @dataclass
    class HandlerOverride:
        """ Simple Wrapped for forced semantic use """
        override : str              = field()
        data     : ValueInterface   = field()


    def __post_init__(self, handlers, structs):
        # TODO init any semantics or structs passed in as Class's
        # TODO check depsem -> struct compabilities
        # by running dependent.compatible(struct)
        # use default sieve if sieve is empty
        if not bool(self.sieve):
            self.sieve += self._default_sieve
        # register provided handlers
        for handler in handlers:
            self._register_handler(handler)

        for struct in structs:
            self._register_struct(struct)


    def _register_handler(self, handler):
        # TODO maybe handle tuples later
        pair_str = handler.mapped_to
        assert(pair_str not in self.registered_handlers)
        self.registered_handlers[pair_str] = handler

    def _register_struct(self, struct):
        # TODO maybe handle tuples later
        pair_str = struct.mapped_to
        assert(pair_str not in self.registered_structs)
        self.registered_structs[pair_str] = struct

    def lookup(self, value: ValueInterface) -> HandlerTuple:
        # sieve from most to least specific
        if value is None:
            return self.default

        for sieve_fn in self.sieve:
            key = sieve_fn(value)
            handler = None
            struct  = None
            if not bool(key):
                continue

            if key in self.registered_handlers:
                handler = self.registered_handlers[key]
            if key in self.registered_structs:
                struct = self.registered_structs[key]

            if handler is None:
                continue

            return (handler, struct)

        # Final resort
        logging.debug(f"Resorting to default handler for: {value}")
        return self.default

    def override(self, new_target: str, value) -> Overrider:
        if new_target not in self.registered_handlers:
            raise AcabBaseException(f"Undefined override handler: {new_target}")

        return HandlerSystemInterface.HandlerOverride(new_target, value)
    @abc.abstractmethod
    def __call__(self, *args, **kwargs):
        pass

#--------------------
@dataclass
class HandlerComponent(metaclass=abc.ABCMeta):
    mapped_to : Sentence = field()
