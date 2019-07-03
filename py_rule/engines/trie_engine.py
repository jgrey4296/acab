"""
The combined engine of underlying trie based knowledge store,
with support for transforms and actions
"""
import logging as root_logger
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import IPython

from py_rule.abstract.EngineBase import EngineBase
from py_rule.abstract import Actions as Actions
from py_rule.abstract import Contexts, Query
from py_rule.abstract import Transforms
from py_rule.abstract import Action
from py_rule.trie import FactBaseTrie
from py_rule.trie.nodes import FactNode
import pyRule.utils as util

from py_rule.fact_trie.parsing import ActionParser as AP
from py_rule.fact_trie.parsing import FactParser as FP
from py_rule.fact_trie.parsing import FileParser as FileP
from py_rule.fact_trie.parsing import QueryParser as QP
from py_rule.fact_trie.parsing import RuleParser as RP
from py_rule.fact_trie.parsing import TransformParser as TP
from py_rule.fact_trie.parsing import TrieRule as TR

#import and register policies

logging = root_logger.getLogger(__name__)

class TrieEngine(EngineBase):
    """ The Engine for an Agent.
    Holds a KnowledgeBase, with rules, keeps track of proposed actions
    and the history of the agent. Performs actions that are registered
    """

    def __init__(self, path=None, init=None):
        super().__init__(Trie, path=path, init=init)

    def load_file(self, filename):
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        assert(exists(filename))
        logging.info("Loading: {}".format(filename))
        with open(filename) as f:
            s = f.read()
        if s is not None:
            #TODO: load (layer, policy) tuples
            #rules, assertions, layers, policies = FileP.parseString(s)
            rules, assertions = FileP.parseString(s)
            #Assert facts:
            for x in assertions:
                logging.info("File load assertion: {}".format(x))
                self.add(x)
            #register rules:
            self.registerRules(rules)
            #register layer sequences
            #register policies to layers
        else:
            raise Exception("No text found in provided file")

    def tick(self, inputMessages):
        ouput = []
        for x in inputMessages:
            self.add(x)

        #loop through layers, running connected policies
        for (layerTags, policy) in self._layers:
            self._run_rules(rule_tags=layer, policy=policy)

        #retrieve final selected actions
        output = self._proposed_actions.copy()
        return output

    #todo: be able to assert retract or query from tries instead of strings
    def add(self, s):
        """ Assert Data into the knowledge base """
        if isinstance(s, str):
            self._knowledge_base.assertS(s)
        else:
            assert(isinstance(s, list))
            assert(isinstance(s[0], FactNode))
            self._knowledge_base.assertFact(s)

    def retract(self, s):
        """ Remove information from the knowledge base """
        if isinstance(s, str):
            self._knowledge_base.retractS(s)
        else:
            assert(isinstance(s, list))
            assert(isinstance(s[0], FactNode))
            self._knowledge_base.retractFact(s)

    def query(self, s):
        """ As a question of the knowledge base """
        if isinstance(s, str):
            return self._knowledge_base.queryS(s)
        else:
            assert(isinstance(s, Query))
            return self._knowledge_base.queryFact(s)

    def registerRules(self, s):
        """ Add a Rule to the engine """
        if isinstance(s, str):
            rules = RP.parseString(s)
        else:
            assert(all([isinstance(x, TR.TrieRule) for x in s]))
            rules = s
        for x in rules:
            assert(isinstance(x, TR.TrieRule))
            assert(x.is_coherent())
            logging.info("Registering Rule: {}".format(x._name))
            ruleName = "".join([str(x) for x in x._name])
            self._rules[ruleName] = x
            #Add the rule position as a fact:
            self.add(x._name)
            #then attach the rule as a meta leaf of the last node of that fact
            self._knowledge_base._last_node.set_meta_leaf(util.META_OP.RULE, x)
