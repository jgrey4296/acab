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
from acab.abstract.interfaces.value import Value_i, Sentence_i
from acab.error.acab_base_exception import AcabBaseException
from acab.modules.engines.util import MaybeBuildOperatorCtx

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
        self.load_modules(*self.modules)
        self.semantics.register_data({"printer": self.printer})
        self.initialised = True

    @EnsureInitialised
    @MaybeBuildOperatorCtx
    def __call__(self, inst:Instruction, bindings=None) -> CtxCon:
        """ Where a inst could be a:
        str to parse then,
        sentence to assert, query, or run
        """
        logging.debug(f"Engine Call on: {inst}")
        if not isinstance(inst, list):
            inst = [inst]

        if isinstance(inst, list) and all([isinstance(x, str) for x in inst]):
            inst = [y for x in inst for y in self._dsl_builder.parse(x)[:]]

        assert(all([isinstance(x, (Value_i, Sentence_i)) for x in inst])), inst
        logging.debug(f"Running: {inst}")
        # pass inst to sem system
        result = bindings
        for clause in inst:
            result = self.semantics(clause, ctxs=result)

            if not bool(result):
                logging.info("Attempt Failed")
                break

        return result

    @property
    def bindings(self):
        return self._cached_bindings


    @EnsureInitialised
    @MaybeBuildOperatorCtx
    def insert(self, s: str, ctxs=None):
        """ Assert a new fact into the engine """
        data = self._dsl_builder.parse(s)
        return self.semantics(*data,
                           ctxs=ctxs)

    @EnsureInitialised
    @MaybeBuildOperatorCtx
    def query(self, s: str, ctxs=None, cache=True):
        """ Ask a question of the working memory """
        instruction = self._dsl_builder.query_parse(s)
        result = self.semantics(instruction,
                             ctxs=ctxs)

        if cache:
            self.add_to_cache(result)
        return result


    def add_to_cache(self, result: CtxCon):
        self._cached_bindings.append(result)
        if len(self._cached_bindings) > self._cache_size:
            self._cached_bindings.pop(0)
