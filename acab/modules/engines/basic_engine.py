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
from acab.abstract.interfaces.dsl import DSL_Fragment_i
from acab.abstract.interfaces.engine import AcabEngine_i, EnsureInitialised
from acab.abstract.interfaces.printing import PrintSystem_i
from acab.abstract.interfaces.semantic import SemanticSystem_i
from acab.abstract.parsing.dsl_builder import DSLBuilder
from acab.error.acab_base_exception import AcabBaseException

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

CtxCon      = 'ContextContainer_i'
Instruction = Union[str, 'Sentence', 'AcabStatement']

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
        loaded_mods = self._module_loader.loaded_modules.values()

        # Initialise DSL
        self._dsl_builder = DSLBuilder(self.parser)
        self._dsl_builder.build_DSL(loaded_mods)

        # extend semantics
        self.semantics.extend(loaded_mods)

        # extend printer
        self.printer.extend(loaded_mods)

        # insert operator sentences / Create root operator context
        ops = [y for x in loaded_mods for y in x.operators]
        self.semantics(*ops)

        # Now Load Text files:
        for x in self.load_paths:
            self.load_file(x)

        self.initialised = True

    @EnsureInitialised
    def __call__(self, inst:Instruction, bindings=None) -> CtxCon:
        """ Where a inst could be a:
        str to parse then,
        sentence to assert, query, or run
        """
        if not isinstance(inst, list):
            inst = [inst]

        if isinstance(inst, list) and all([isinstance(x, str) for x in inst]):
            inst = [y for x in inst for y in self._dsl_builder.parse(x)[:]]

        logging.info(f"Running: {inst}")
        # pass inst to sem system
        result = bindings
        for elem in inst:
            result = self.semantics(elem, ctxs=result)

            if not bool(result):
                logging.info("Attempt Failed")
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
        result = self.semantics(instruction, ctxs=ctxs)
        if cache:
            self.add_to_cache(result)
        return result


    def add_to_cache(self, result: CtxCon):
        self._cached_bindings.append(result)
        if len(self._cached_bindings) > self._cache_size:
            self._cached_bindings.pop(0)
