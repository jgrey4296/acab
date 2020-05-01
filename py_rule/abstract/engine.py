""" EngineBase: The Core Interface and implementation independent code for the
    production systems

Engine's are the main programming interface.
You create one with a working memory, load some modules,
and can then parse and run an agent DSL pipeline.
"""
import logging as root_logger
from os.path import exists, split, expanduser, abspath
from py_rule import util

from . import action
from .agenda import Agenda
from .rule import Rule
from .working_memory import WorkingMemory
from .production_operator import ProductionContainer


logging = root_logger.getLogger(__name__)


class Engine:
    """ The Abstract class of a production system engine. """

    def __init__(self, wm_constructor, modules=None, path=None, init=None):
        assert(issubclass(wm_constructor, WorkingMemory))
        self.__kb_constructor = wm_constructor
        self._working_memory = wm_constructor(init)
        self._pipeline = None
        # to be updated with printed representations
        # of the kb state after each action
        self._prior_states = []
        # named recall states of past kb states
        self._recall_states = {}
        # modules
        self._loaded_modules = {}
        # cached bindings
        self._cached_bindings = []

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

    # Initialisation:
    def load_modules(self, *modules):
        self._loaded_modules.update({x.__class__.__name__ : x for x in modules})
        self._working_memory.add_modules(self._loaded_modules.values())
        Agenda.construct_subclass_tree()
        self._working_memory.build_operator_parser()

    def load_file(self, filename):
        """ Load a file spec for the facts / rules / layers for this engine """
        raise NotImplementedError("Base Engine Stub")

    def save_file(self, filename):
        """ Dump the content of the kb to a file to reload later """
        assert(exists(split(abspath(expanduser(filename)))[0]))
        with open(abspath(expanduser(filename)), 'w') as f:
            f.write(str(self._working_memory) + "\n")

    # Base Actions
    def add(self, s):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        self._working_memory.add(s)

    def query(self, s, ctxs=None, cache=True):
        """ As a question of the working memory """
        result = self._working_memory.query(s, ctxs=ctxs)
        if cache:
            self._cached_bindings = result
        return result

    # Export
    def _save_state(self, data):
        """ Copy the current string representation of the working memory,
        and any associated data """
        self._prior_states.append((str(self._working_memory), data))

    # Utility
    def run_thing(self, thing, bindings=None):
        """ Where a thing could be an:
        rule/agenda/layer/pipeline,
        action/query/transform
        """
        # TODO: if thing is string, query it
        # TODO be able to run multi / sequence of things
        # TODO should save state
        assert(isinstance(thing, ProductionContainer))
        logging.info("Running thing: {}".format(thing))

        result = thing(ctxs=bindings, engine=self)

        if not bool(result):
            logging.info("Thing Failed")

        return result

    def __len__(self):
        raise NotImplementedError()
