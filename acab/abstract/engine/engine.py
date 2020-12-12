"""
EngineBase: The Core Interface and implementation independent code for the production systems

Engine's are the main programming dsl_fragments.
You create one with a working memory, load some modules,
and can then parse and run an agent DSL pipeline.
"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.interfaces import engine_interface as EI
from acab.abstract.interfaces.working_memory_interface import WorkingMemoryCore
from acab.abstract.containers.production_abstractions import ProductionOperator, ProductionContainer
from acab.error.acab_base_exception import AcabBaseException
from acab.error.acab_import_exception import AcabImportException
from acab.modules.structures.trie.trie import Trie

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from importlib import import_module
from os.path import exists, split, expanduser, abspath
from re import Pattern
from typing import Callable, Iterator, Union, Match
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from uuid import uuid1, UUID
from weakref import ref
import logging as root_logger

config = AcabConfig.Get()

logging = root_logger.getLogger(__name__)

@dataclass
class Engine(EI.EngineInterface):
    """ The Abstract class of a production system engine. """

    # Blocks engine use until build_DSL has been called
    __wm_constructor      : 'Callable'        = field()
    _loaded_DSL_fragments : Dict[Any, Any]    = field(default_factory=dict)
    _loaded_modules       : Set[Any]          = field(default_factory=set)
    _working_memory       : WorkingMemoryCore = field(init=False)
    init_strs             : List[str]         = field(default_factory=list)
    initialised           : bool              = field(init=False, default=False)
    load_paths            : List[str]         = field(default_factory=list)
    modules               : List[str]         = field(default_factory=list)

    def __post_init__(self):
        assert(callable(self.__wm_constructor))

        self._working_memory = self.__wm_constructor(self.init_strs)

        # TODO  update with reloadable state of working memory
        self._prior_states = []
        # named recall states of past kb states
        self._recall_states = {}
        # cached bindings
        self._cached_bindings = []

        # TODO use these to enable breakpoint context:
        self._current_layer     = None
        self._current_rule      = None
        self._current_query     = None
        self._current_transform = None
        self._current_action    = None

        # initialise
        if bool(self.modules):
            self.load_modules(*self.modules)

        if bool(self.load_paths):
            for x in self.load_paths:
                self.load_file(x)


    @property
    def bindings(self):
        return self._cached_bindings

    # Initialisation:
    def reload_all_modules(self):
        loaded = list(self._loaded_modules)
        self._loaded_modules.clear()
        self._load_modules(loaded)

    def load_modules(self, *modules: List[str]):
        """ Given ModuleInterface objects,
        store them then tell the working memory to load them
        return a list of dictionaries
        """
        return [self.load_module_values(x) for x in modules]

    def load_module_values(self, module_str: str):
        """
        Load a module, extract operators and dsl fragments from it,
        put the operators into the operators store,
        register the dsl fragments for later use

        Returns a working_memory query result of the module
        """
        # Prepare path
        # TODO use utility constants for joining and query
        if not isinstance(module_str, str):
            breakpoint()
            raise Exception("TODO: handle sentence -> str")
        # print semantics: basic+word join of "."
        # mod_str = str(module_sen)

        # Return early if already loaded
        if module_str in self._loaded_modules:
            logging.info("Module already loaded: {}".format(module_str))
            # TODO extract node from return context?
            return self._working_memory.query(module_str + "?")

        # Load
        try:
            the_module = import_module(module_str)
        except ModuleNotFoundError as e:
            breakpoint()

            raise AcabImportException(module_str) from None

        # Extract
        operator_sentences, dsl_fragments = self.extract_from_module(the_module)

        # Register DSL Fragments
        self._loaded_DSL_fragments[module_str] = dsl_fragments
        self.register_ops(operator_sentences)

        self._loaded_modules.add(module_str)
        # TODO extract node from return context?
        return self._working_memory.query(module_str+ "?")

    def build_DSL(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        self._working_memory.clear_bootstrap()
        self._working_memory.construct_parsers_from_fragments([y for x in self._loaded_DSL_fragments.values() for y in x])
        self.initialised = True




    def load_file(self, filename):
        """ Load a file spec for the facts / rules / layers for this engine """
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")

        return self._load_file(filename)

    def save_file(self, filename):
        """ Dump the content of the kb to a file to reload later """
        # TODO control with print semantics
        assert(exists(split(abspath(expanduser(filename)))[0]))
        with open(abspath(expanduser(filename)), 'w') as f:
            f.write(str(self._working_memory) + "\n")

    def _save_state(self, data):
        """ Copy the current string representation of the working memory,
        and any associated data """
        # TODO replace this with a down
        self._prior_states.append((str(self._working_memory), data))


    # Base Actions
    def add(self, s):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")
        self._working_memory.add(s)

    def query(self, s, ctxs=None, cache=True):
        """ Ask a question of the working memory """
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")
        result = self._working_memory.query(s, ctxs=ctxs, engine=self)
        if cache:
            self._cached_bindings = result
        return result


    def add_listeners(self, *words):
        """ Add basic data breakpoints """
        self._working_memory.register_listeners(words)

    def remove_listeners(self, *words):
        """ Remove data breakpoints """
        self._working_memory.unregister_listeners(words)

    def set_listener_threshold(self, a, b):
        """ Specify the number of word matches
        are needed to trigger the breakpoint """
        self._working_memory.set_listener_threshold(a, b)

    def get_listeners(self):
        return self._working_memory._listeners

    def get_listener_threshold(self):
        return self._working_memory._listener_threshold


    # Utility
    def __call__(self, thing, bindings=None):
        """ Where a thing could be an:
        rule/agenda/layer/pipeline,
        action/query/transform
        """
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")
        result = False
        # if thing is string, query it
        if isinstance(thing, str):
            thing = [thing]

        if isinstance(thing, list) and all([isinstance(x, str) for x in thing]):
            result = [self._working_memory(x) for x in thing]

        else:
            assert(isinstance(thing, ProductionContainer))
            logging.info("Running thing: {}".format(thing))
            # TODO: this will use a production semantics
            result = thing(ctxs=bindings, engine=self)

        if not bool(result):
            logging.info("Thing Failed")

        return result

    def to_sentences(self):
        """
        Triggers the working memory to produce a full accounting,
        in canonical style (able to be used by typechecker)
        All statements are output as leaves,
        and all paths with non-leaf statements convert to simple formats
        """
        return self._working_memory.to_sentences()

