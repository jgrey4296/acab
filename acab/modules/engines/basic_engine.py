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
                    Set, Tuple, Type, TypeVar, Union, cast)

from acab.core.config.config import AcabConfig
from acab.core.data.instruction import ProductionContainer, ProductionOperator
from acab.core.decorators.engine import (EnsureEngineInitialised,
                                         MaybeBuildOperatorCtx)
from acab.core.engine.engine_base import AcabEngineImpl
from acab.core.engine.module_loader import ModuleLoader
from acab.core.parsing.pyparse_dsl import PyParseDSL
from acab.error.base import AcabException
from acab.interfaces.dsl import DSL_Builder_i, DSL_Fragment_i
from acab.interfaces.engine import AcabEngine_i
from acab.interfaces.printing import PrintSystem_i
from acab.interfaces.semantic import SemanticSystem_i
from acab.interfaces.value import Sentence_i, Value_i

logging = root_logger.getLogger(__name__)
config = AcabConfig()

CtxSet      = 'ContextSet_i'
Instruction = 'str|Sentence|Instruction'

@dataclass
class AcabBasicEngine(AcabEngineImpl, AcabEngine_i):
    """ The Abstract class of a production system engine. """
    # Blocks engine use until build has been called:
    dsl_builder      : Type[DSL_Builder_i] = field(init=False, default=PyParseDSL)
    _module_loader   : ModuleLoader        = field(init=False, default_factory=ModuleLoader)
    # LIFO size limited cache:
    _cached_bindings : list[Any]    = field(init=False, default_factory=list)
    _cache_size      : int          = field(default=10)

    def __post_init__(self):
        # initialise modules
        self.load_modules(*self.modules)
        self.semantics.register({"printer": self.printer})
        self.initialised = True

    def __repr__(self):
        clsname = self.__class__.__name__
        parser_base = self.parser.__class__.__name__
        semantics = repr(self.semantics)
        printer = repr(self.printer)
        modules = repr(self._module_loader)
        return f"{clsname}(\n-- ({parser_base})\n-- {semantics}\n-- {printer}\n-- {modules}\n)"

    @EnsureEngineInitialised
    @MaybeBuildOperatorCtx
    def __call__(self, inst:Instruction, ctxset=None) -> CtxSet:
        """ Where a inst could be a:
        str to parse then,
        sentence to assert, query, or run
        """
        logging.debug(f"Engine Call on: {inst}")
        if not isinstance(inst, list):
            inst = [inst]

        if isinstance(inst, list) and all([isinstance(x, str) for x in inst]):
            inst = [y for x in inst for y in self._dsl.parse(x)[:]]

        assert(all([isinstance(x, (Value_i, Sentence_i)) for x in inst])), inst
        logging.debug(f"Enacting Instruction: {str(inst)}")
        return self.semantics(*inst, ctxs=ctxset)

    @property
    def bindings(self):
        return self._cached_bindings


    @EnsureEngineInitialised
    def add_to_cache(self, result: CtxSet):
        self._cached_bindings.append(result)
        if len(self._cached_bindings) > self._cache_size:
            self._cached_bindings.pop(0)


    @staticmethod
    def from_sentences(self, sens):
        # TODO make the engine, then load sens
        # for sen in sens:
        #     self(sen)

        # return self
        raise NotImplementedError()

    def to_word(self):
        raise NotImplementedError()
