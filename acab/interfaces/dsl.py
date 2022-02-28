"""
A DSL interface for the system, which

"""
import abc
import collections.abc as cABC
import logging as root_logger
import traceback
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping,  Sequence, TypeAlias,
                    Tuple, TypeVar, cast, TYPE_CHECKING)

if TYPE_CHECKING:
    import io

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.core.decorators.dsl import EnsureDSLInitialised
from acab.error.base import AcabBasicException
from acab.error.parse import AcabParseException
from acab.interfaces.handler_system import (Handler, Handler_Fragment, _HandlerSpec_d,
                                            HandlerSpec, HandlerSystem_i)
from acab.interfaces.handler_system import Handler_Fragment, Handler, HandlerSpec

Parser           : TypeAlias = AT.Parser
Sentence         : TypeAlias = AT.Sentence
Query            : TypeAlias = AT.Container
ModuleComponents : TypeAlias = AT.ModuleComponents
DSL_Spec_A       : TypeAlias = "DSL_Spec_i"
File             : TypeAlias = 'io.TextIOBase'

# Data ########################################################################
@dataclass
class DSL_Handler_i(Handler):
    """ Register a function for handling a DSL setup signal.
    ie: This function is run to set a pyparsing `Forward`"""

    func : Parser = field()

@dataclass
class _DSL_Spec_d(_HandlerSpec_d):
    """
    Register a signal into a DSL,

    """
    struct : set[Parser] = field(default_factory=set)

@dataclass
class _DSL_Builder_d:

    _parsers_initialised  : bool           = field(init=False, default=False)
    _loaded_DSL_fragments : dict[Any, Any] = field(init=False, default_factory=dict)

# Protocols ###################################################################
class DSL_Fragment(Handler_Fragment):
    """ """

    def parseString(self, string):
        """ Takes a String, parses it into Data format """
        # TODO maybe create a parser on demand of all handlers in the fragment?
        raise NotImplementedError()

    def parse_file(self, file):
        raise NotImplementedError()


class DSL_Spec_i(HandlerSpec, _DSL_Spec_d):

    def extend_spec(self, spec:DSL_Spec_A):
        if not isinstance(spec, DSL_Spec_i):
            raise AcabParseException(f"Tried to extend a DSL_Spec with {spec}")

        self.struct.update(spec.struct)

    def register(self, handler):
        assert(isinstance(handler, DSL_Handler_i))
        if handler.func is None:
            raise AcabParseException(f"`{self.signal}` attempted to register a handler without a parser")

        # TODO flag logic, api's
        self.handlers.append(handler)

    @abc.abstractmethod
    def parseString(self, *args, **kwargs):
        pass

    @abc.abstractmethod
    def build(self):
        pass

class DSL_Builder_i(HandlerSystem_i, _DSL_Builder_d):

    def _register_spec(self, *specs):
        for spec in specs:
            assert(isinstance(spec, DSL_Spec_i))
            if spec.signal not in self.handler_specs:
                self.handler_specs[spec.signal] = spec
            else:
                self.handler_specs[spec.signal].extend_spec(spec)

    def build(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        if bool(self.loose_handlers):
            loose_handlers      = self.loose_handlers[:]
            self.loose_handlers = []
            for loose in loose_handlers:
                self.register(loose)

        if bool(self.loose_handlers):
            loose_signals = ", ".join(set([x.signal for x in self.loose_handlers]))
            logging.warning(f"Building DSL with {len(self.loose_handlers)} Loose Handlers: {loose_signals}")
        else:
            logging.info("Building DSL")

        for spec in self:
            try:
                spec()
            except TypeError as err:
                breakpoint()

        self._parsers_initialised = True

    def parseString(self, *args) -> list[Sentence]:
        return self.parse(*args)

    def parseFile(self, f:str) -> list[Sentence]:
        logging.debug(f"Loading File: {f}")
        text = ""
        with open(f, "r") as the_file:
            text = the_file.read()

        logging.info(f"Loading File Text:\n{text}")
        return self.parse(text)


    def __call__(self, *args):
        return self.parse(*args)

    def extend(self, modules:list[ModuleComponents]):
        for module in modules:
            self.register(*module.dsl_fragments)

    def __repr__(self):
        return f"<{self.__class__.__name__}: Handlers: {len(self.handler_specs)}, Loose: {len(self.loose_handlers)}>"
    @abc.abstractmethod
    @EnsureDSLInitialised
    def parse(self, s:str) -> list[Sentence]:
        pass

    @abc.abstractmethod
    def _register_default(self):
        pass
