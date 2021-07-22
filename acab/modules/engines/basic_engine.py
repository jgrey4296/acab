"""
EngineBase: The Core Interface and implementation independent code for the production systems

Engine's are the main programming dsl_fragments.
You create one with a working memory, load some modules,
and can then parse and run an agent DSL pipeline.
"""
import logging as root_logger
from dataclasses import dataclass, field
from os.path import abspath, exists, expanduser, split
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.production_abstractions import (ProductionContainer,
                                                        ProductionOperator)
from acab.abstract.engine.module_loader import ModuleLoader
from acab.abstract.interfaces.dsl_interface import DSL_Interface
from acab.abstract.interfaces.engine_interface import AcabEngine_i, EnsureInitialised
from acab.abstract.interfaces.printing_interfaces import PrintSystem_i
from acab.abstract.interfaces.semantic_interfaces import SemanticSystem_i
from acab.abstract.parsing.dsl_builder import DSLBuilder
from acab.error.acab_base_exception import AcabBaseException

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

CtxCon = 'CtxContainer'

@dataclass
class AcabBasicEngine(AcabEngine_i):
    """ The Abstract class of a production system engine. """
    # Blocks engine use until build_DSL has been called:
    _module_loader   : ModuleLoader = field(init=False, default_factory=ModuleLoader)
    # LIFO size limited cache:
    _cached_bindings : List[Any]    = field(init=False, default_factory=list)
    _cache_size      : int          = field(default=10)

    def __post_init__(self):
        # initialise modules
        self._module_loader.load_modules(*self.modules)

        # Initialise DSL
        self._dsl_builder = DSLBuilder(self.parser)
        self._dsl_builder.build_DSL(self._module_loader.loaded_modules.values())

        # Now Load Text files:
        for x in self.load_paths:
            self.load_file(x)

        self.initialised = True

    @EnsureInitialised
    def __call__(self, thing, bindings=None):
        """ Where a thing could be a:
        str to parse then,
        sentence to assert, query to ask,
        or abstraction to run
        """
        result = False
        # TODO if thing is string, parse it
        if not isinstance(thing, list):
            thing = [thing]

        if isinstance(thing, list) and all([isinstance(x, str) for x in thing]):
            # TODO parse
            thing = [self.dsl_builder.parse(x) for x in thing]

        assert(isinstance(thing, ProductionContainer))
        logging.info("Running thing: {}".format(thing))
        # TODO pass instruction to sem system
        result = None
        for elem in thing:
            result = self.semantics(elem, ctxs=result)

            if not bool(result):
                logging.info("Thing Failed")
                break

        return result

    @property
    def bindings(self):
        return self._cached_bindings


    @EnsureInitialised
    def insert(self, s: str, ctxs=None):
        """ Assert a new fact into the engine """
        data = self._dsl_builder.parse(s)
        return self.semantics(*data, ctxs=ctxs)

    @EnsureInitialised
    def query(self, s: str, ctxs=None, cache=True):
        """ Ask a question of the working memory """
        instruction = self._dsl_builder.query_parse(s)
        # TODO ensure instruction is a query?
        result = self.semantics(instruction, ctxs=ctxs)
        if cache:
            self.add_to_cache(result)
        return result


    def add_to_cache(self, result: CtxCon):
        self._cached_bindings.append(result)
        if len(self._cached_bindings) > self._cache_size:
            self._cached_bindings.pop(0)
