"""
The abstract form of a working memory

Working Memory is responsible for the core system capabilities.
It provides an ontology structure, parsers for that structure,
and integrates modules into those parsers.

The canonical working memory is the Trie_WM.
It provides an Exclusion Logic Trie data structure,
Parsers to define sentences and rules in that data structure,
and can load modules to extend its capabilities.

The working memory, at core, can Add, Retract, and Query facts.

From a module it loads Value, Statement, and Annotation parsers.

"""
import pyparsing as pp
import logging as root_logger
from fractions import Fraction

from .production_operator import ProductionOperator
from .dsl_fragment import DSL_Fragment
from .bootstrap_parser import BootstrapParser

logging = root_logger.getLogger(__name__)


class WorkingMemory:
    """ The Abstract Working Memory """

    def __init__(self, init):
        self._have_added_types = False
        self._have_built_operators = False
        self._module_hotload_provision = {}

        # Use a Bootstrap DSL for specification
        self._bootstrap_parser = BootstrapParser()
        # Listeners are treated as a query *bag*
        self._listeners = set()
        self._listener_threshold = Fraction(1,2)


    def __str__(self):
        """ Print the working memory as a reparseable string """
        raise NotImplementedError()

    def __eq__(self, other):
        raise NotImplementedError()


    def clear_listeners(self):
        self._listeners = set()

    def register_listeners(self, words):
        self._listeners.update(words)

    def unregister_listeners(self, words):
        self._listeners.difference_update(words)

    def set_listener_threshold(self, a, b):
        self._listener_threshold = Fraction(a,b)

    def score_listener(self, words):
        simple_words = [str(x) if not x.is_var else "$_" for x in words]
        num_in_listener_bag = sum([1 if x in self._listeners else 0 for x in simple_words])
        sentence_fraction = Fraction(num_in_listener_bag, len(simple_words))
        if sentence_fraction >= self._listener_threshold:
            return True

        return False

    def breakpoint(self):
        # TODO: add more listener options: pre, on and post
        breakpoint()

    def to_sentences(self):
        return NotImplementedError()


    def construct_parsers_from_fragments(self, fragments):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        assert(all([isinstance(x, DSL_Fragment) for x in fragments]))
        self.assert_parsers(self._bootstrap_parser)
        #Populate the trie
        dummy = [x.assert_parsers(self._bootstrap_parser) for x in fragments]

        # Now query and populate the modules
        dummy = [x.query_parsers(self._bootstrap_parser) for x in fragments]

        self.query_parsers(self._bootstrap_parser)

    def clear_bootstrap(self):
        self._bootstrap_parser = BootstrapParser()


    # Methods to implement:
    def assert_parsers(self, parser_trie):
        """ Assert base parser fragments of the wm's DSL """
        raise NotImplementedError()

    def query_parsers(self, parser_trie):
        """ Retrieve parser fragments to finalize the wm's DSL """
        raise NotImplementedError()


    def add(self, data, leaf=None):
        """
        Add a sentence to the working memory.
        If leaf is specified, it is an AcabValue which will be attached
        as the leaf, with the sentence's name.
        eg: add(a.test.sentence.x, leaf=some_value) -> a.test.sentence.x(::some_value)
        """
        raise NotImplementedError()

    def query(self, ctxs=None, engine=None):
        raise NotImplementedError()


    # Deprecated
    def retract(self, data):
        raise DeprecationWarning()

    def add_modules(self, mods):
        raise DeprecationWarning("add_modules is deprecated use construct_parsers_from_fragments")

