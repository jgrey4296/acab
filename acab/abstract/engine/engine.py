"""
EngineBase: The Core Interface and implementation independent code for the production systems

Engine's are the main programming dsl_fragments.
You create one with a working memory, load some modules,
and can then parse and run an agent DSL pipeline.
"""
import logging as root_logger
from os.path import exists, split, expanduser, abspath
from importlib import import_module

from acab.config import AcabConfig
from acab.error.acab_import_exception import AcabImportException
from acab.error.acab_base_exception import AcabBaseException

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence

from acab.abstract.rule.production_operator import ProductionOperator, ProductionContainer
from acab.abstract.rule import action
from acab.abstract.rule.rule import Rule

from acab.abstract.pipeline.agenda import Agenda

from acab.abstract.engine.dsl_fragment import DSL_Fragment
from acab.abstract.engine.working_memory import WorkingMemory

from acab.modules.structures.trie.trie import Trie

util = AcabConfig.Get()

logging = root_logger.getLogger(__name__)


class Engine:
    """ The Abstract class of a production system engine. """

    def __init__(self, wm_constructor, op_constructor=None, modules=None, path=None, init=None):
        assert(callable(wm_constructor))
        if op_constructor is None:
            op_constructor = Trie

        assert(callable(op_constructor))

        # Blocks engine use until build_DSL has been called
        self._initialised = False

        # TODO: enable separated working memories for separate layers
        self.__wm_constructor = wm_constructor

        self._working_memory = wm_constructor(init)
        # modules
        self._loaded_modules = set()
        self._loaded_DSL_fragments = {}

        # TODO  update with reloadable state of working memory
        self._prior_states = []
        # named recall states of past kb states
        self._recall_states = {}
        # cached bindings
        self._cached_bindings = []

        # TODO use these to enable breakpoint context:
        self._current_layer = None
        self._current_rule = None
        self._current_query = None
        self._current_transform = None
        self._current_action = None

        # initialise
        if modules is not None:
            self.load_modules(*modules)

        if path is None:
            logging.info("Not loading any files for the working memory")
        elif isinstance(path, list):
            for x in path:
                self.load_file(x)
        else:
            self.load_file(path)


    @property
    def bindings(self):
        return self._cached_bindings

    # Initialisation:
    def load_modules(self, *modules):
        """ Given ModuleInterface objects,
        store them then tell the working memory to load them
        return a list of dictionaries
        """
        return [self.load_module_values(x) for x in modules]

    def load_module_values(self, module_sen):
        """
        Load a module, extract operators and dsl fragments from it,
        put the operators into the operators store,
        register the dsl fragments for later use

        Returns a working_memory query result of the module
        """
        # Prepare path
        # TODO use utility constants for joining and query
        mod_str = module_sen
        if not isinstance(mod_str, str):
            mod_str = module_sen.pprint(seq_join=".")

        # Return early if already loaded
        if module_sen in self._loaded_modules:
            logging.info("Module already loaded: {}".format(module_sen))
            # TODO extract node from return context?
            return self._working_memory.query(module_sen + "?")

        # Load
        try:
            the_module = import_module(mod_str)
        except ModuleNotFoundError as e:
            raise AcabImportException(module_sen) from None

        # Extract
        operator_sentences, dsl_fragments = self.extract_from_module(the_module)

        # Register DSL Fragments
        self._loaded_DSL_fragments[module_sen] = dsl_fragments
        self.register_ops(operator_sentences)

        self._loaded_modules.add(module_sen)
        # TODO extract node from return context?
        if isinstance(module_sen, str):
            return self._working_memory.query(module_sen + "?")
        else:
            return self._working_memory.query(module_sen)

    def build_DSL(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        self._working_memory.clear_bootstrap()
        self._working_memory.construct_parsers_from_fragments([y for x in self._loaded_DSL_fragments.values() for y in x])
        self._initialised = True


    def register_ops(self, sentences):
        """
        Assert sentences into the operator working memory
        """
        raise NotImplementedError()

    def alias_module(self, mod_name, alias_name):
        """
        Assert an alias of a module into the operator wm
        """
        raise NotImplementedError()

    def get_operator(self, op_name):
        """ Get an operator from the operator wm """
        raise NotImplementedError()


    def load_file(self, filename):
        """ Load a file spec for the facts / rules / layers for this engine """
        if not self._initialised:
            raise AcabBaseException("DSL Not Initialised")

        return self._load_file(filename)

    def _load_file(self, filename):
        raise NotImplementedError("Base Engine Stub")

    def save_file(self, filename):
        """ Dump the content of the kb to a file to reload later """
        assert(exists(split(abspath(expanduser(filename)))[0]))
        with open(abspath(expanduser(filename)), 'w') as f:
            f.write(str(self._working_memory) + "\n")

    def _save_state(self, data):
        """ Copy the current string representation of the working memory,
        and any associated data """
        self._prior_states.append((str(self._working_memory), data))


    # Base Actions
    def add(self, s):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        if not self._initialised:
            raise AcabBaseException("DSL Not Initialised")
        self._working_memory.add(s)

    def query(self, s, ctxs=None, cache=True):
        """ Ask a question of the working memory """
        if not self._initialised:
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
        if not self._initialised:
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

    def extract_from_module(self, module):
        raise NotImplementedError()

    # Deprecated
    def reload_all_modules(self):
        raise DeprecationWarning("Use build_DSL")
