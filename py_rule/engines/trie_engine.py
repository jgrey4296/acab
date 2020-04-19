"""
The combined engine of underlying trie based working memory,
with support for transforms and actions
"""
import logging as root_logger
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os.path import abspath
from os import listdir
from random import shuffle
from py_rule.error.pyrule_parse_exception import PyRuleParseException

from py_rule.abstract import action, contexts, query, transform
from py_rule.abstract.engine import Engine
from py_rule.abstract.sentence import Sentence
from py_rule.working_memory.trie_wm.trie_working_memory import TrieWM
from py_rule.working_memory.trie_wm.nodes.fact_node import FactNode
import py_rule.util as util

from py_rule.working_memory.trie_wm.parsing import ActionParser as AP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.working_memory.trie_wm.parsing import TotalParser as TotalP
from py_rule.working_memory.trie_wm.parsing import QueryParser as QP
from py_rule.working_memory.trie_wm.parsing import RuleParser as RP
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.working_memory.trie_wm import trie_rule as TR

# import and register policies

logging = root_logger.getLogger(__name__)


class TrieEngine(Engine):
    """ The Engine for an Agent.
    Holds a working memory, with rules, keeps track of proposed actions
    and the history of the agent. Performs actions that are registered
    """

    def __init__(self, modules=None, path=None, init=None):
        super().__init__(TrieWM, modules=modules, path=path, init=init)

    def load_file(self, filename):
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assert exists(filename), filename
        with open(filename) as f:
            the_string = f.read()
        if the_string is not None:
            # everything should be an assertion
            assertions = TotalP.parseString(the_string)
            # Assert facts:
            for x in assertions:
                logging.info("File load assertions: {}".format(x))
                self.add(x)

            # TODO register layer sequences
            # TODO register policies to layers
        else:
            raise PyRuleParseException("No text found in provided file")

        # TODO: return a response about the loading

    def tick(self, inputMessages):
        # TODO fix this
        ouput = []
        for x in inputMessages:
            self.add(x)

        for layer in self._pipeline._layers:
            self.run_layer(layer)

        # retrieve final selected actions
        output = self._proposed_actions.copy()
        return output
