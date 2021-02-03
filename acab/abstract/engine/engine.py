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
from acab.abstract.interfaces.data_interfaces import StructureInterface
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionContainer
from acab.error.acab_base_exception import AcabBaseException
from acab.error.acab_import_exception import AcabImportException
from acab.error.acab_parse_exception import AcabParseException
from acab.modules.structures.trie.trie import Trie
from acab.abstract.interfaces.dsl_interface import DSL_Interface
from acab.abstract.parsing.bootstrap_parser import BootstrapParser

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from importlib import import_module
from os.path import exists, split, expanduser, abspath
from re import Pattern, compile
from typing import Callable, Iterator, Union, Match
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from uuid import uuid1, UUID
from weakref import ref
import logging as root_logger

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

MODULE_SPLIT_REG = compile(config.value("Parse.Patterns", "MODULE_SPLIT_REG"))


@dataclass
class Engine(EI.RewindEngineInterface, EI.ModuleLoaderInterface, EI.DSLBuilderInterface):
    """ The Abstract class of a production system engine. """

    # Blocks engine use until build_DSL has been called
    _wm_constructor : Callable           = field(init=False)
    _working_memory : StructureInterface = field(init=False)
    init_strs       : List[str]          = field(default_factory=list)
    initialised     : bool               = field(init=False, default=False)
    load_paths      : List[str]          = field(default_factory=list)

    def __post_init__(self):
        assert(callable(self.__wm_constructor))
        self._working_memory = self.__wm_constructor(self.init_strs)
        # cached bindings
        self._cached_bindings = []

        # initialise modules
        if bool(self.modules):
            self.load_modules(*self.modules)

        # Initialise DSL
        self.build_DSL()

        # Now Load Text files:
        if bool(self.load_paths):
            for x in self.load_paths:
                self.load_file(x)



    @property
    def bindings(self):
        return self._cached_bindings

    # Initialisation:

    def load_file(self, filename):
        """ Load a file spec for the facts / rules / layers for this engine """
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")

        return self._load_file(filename)

    def _load_file(self, filename):
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assert exists(filename), filename
        with open(filename) as f:
            # everything should be an assertion
            try:
                assertions = self._main_parser.parseFile(f)
            except pp.ParseException as exp:
                print("-----")
                print(str(exp))
                print(exp.markInputline())
                print("File Not Asserted into WM")
                return False

            # Assert facts:
            for x in assertions:
                logging.info("File load assertions: {}".format(x))
                self.add(x)

        return True

    def save_file(self, filename, printer):
        """ Dump the content of the kb to a file to reload later """
        # TODO add default print semantics
        assert(exists(split(abspath(expanduser(filename)))[0]))
        as_sentences = self._working_memory.to_sentences()
        as_strings = printer(as_sentences)
        with open(abspath(expanduser(filename)), 'w') as f:
            f.write(as_strings)

    def add(self, s: str):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")

        data = self._main_parser.parseString(s)
        self._working_memory.add(data)

    def query(self, s: str, ctxs=None, cache=True):
        """ Ask a question of the working memory """
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")
        data = self._query_parser.parseString(s)
        result = self._working_memory.query(data, ctxs=ctxs, engine=self)
        if cache:
            self._cached_bindings = result
        return result


    # TODO  rework these

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
        # TODO use a semantics + down
        return self._working_memory.to_sentences()


