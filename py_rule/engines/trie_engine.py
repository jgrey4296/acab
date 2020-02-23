"""
The combined engine of underlying trie based knowledge store,
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
from py_rule.knowledge_bases.trie_kb.trie_knowledge_base import TrieKnowledgeBase
from py_rule.knowledge_bases.trie_kb.nodes.fact_node import FactNode
import py_rule.util as util

from py_rule.knowledge_bases.trie_kb.parsing import ActionParser as AP
from py_rule.knowledge_bases.trie_kb.parsing import FactParser as FP
from py_rule.knowledge_bases.trie_kb.parsing import TotalParser as FileP
from py_rule.knowledge_bases.trie_kb.parsing import QueryParser as QP
from py_rule.knowledge_bases.trie_kb.parsing import RuleParser as RP
from py_rule.knowledge_bases.trie_kb.parsing import TransformParser as TP
from py_rule.knowledge_bases.trie_kb import trie_rule as TR

# import and register policies

logging = root_logger.getLogger(__name__)


class TrieEngine(Engine):
    """ The Engine for an Agent.
    Holds a KnowledgeBase, with rules, keeps track of proposed actions
    and the history of the agent. Performs actions that are registered
    """

    def __init__(self, path=None, init=None):
        super().__init__(TrieKnowledgeBase, path=path, init=init)

    def load_file(self, filename):
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assert exists(filename), filename
        with open(filename) as f:
            s = f.read()
        if s is not None:
            # TODO: load (layer, policy) tuples
            # rules, assertions, layers, policies = FileP.parseString(s)
            rules, assertions = FileP.parseString(s)
            # Assert facts:
            for x in assertions:
                logging.info("File load assertions: {}".format(x))
                self.add(x)

            # register layer sequences
            # register policies to layers
        else:
            raise PyRuleParseException("No text found in provided file")

    def tick(self, inputMessages):
        ouput = []
        for x in inputMessages:
            self.add(x)

        # loop through layers, running connected policies
        for (layerTags, policy) in self._layers:
            self._run_rules(rule_tags=layerTags, policy=policy)

        # retrieve final selected actions
        output = self._proposed_actions.copy()
        return output

