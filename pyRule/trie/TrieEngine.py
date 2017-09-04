"""
The combined engine of underlying trie based knowledge store,
with support for transforms and actions
"""
import logging as root_logger
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
from random import shuffle

import  pyRule.utils as util
from pyRule import Actions as Actions
from pyRule import Transforms
from pyRule import Contexts, Rule, Query
from pyRule.trie import Trie, Node
from pyRule.Actions import Action
from pyRule.EngineBase import EngineBase

from . import TransformParser as TP
from . import ActionParser as AP
from . import QueryParser as QP
from . import FactParser as FP
from . import RuleParser as RP
from . import FileParser as FileP

import IPython

logging = root_logger.getLogger(__name__)

class TrieEngine(EngineBase):
    
    def __init__(self, path=None, init=None):
        super().__init__(Trie, path=path, init=init)
        self._knowledge_base = Trie(init)
        self._rules = {}
        self._proposed_actions = []
        #to be updated with printed representations of the trie state after each action
        self._prior_states = []
        #named recall states of past tries
        self._recall_states = []
        #Registered custom actions
        self._custom_actions = {}
        if path is not None:
            self.load_file(path)

    def load_file(self, filename):
        assert(isinstance(filename, str))
        assert(exists(filename))
        logging.info("Loading: {}".format(filename))
        with open(filename) as f:
            s = f.read()
        if s is not None:
            rules, assertions = FileP.parseString(s)
            for x in assertions:
                logging.info("File load assertion: {}".format(x))
                self.add(x)
            self.registerRules(rules)            
        else:
            raise Exception("No text found in provided file")
 

    #todo: be able to assert retract or query from tries instead of strings
    def add(self, s):
        if isinstance(s, str):
            self._knowledge_base.assertS(s)
        else:
            assert(isinstance(s[0],Node))
            self._knowledge_base.assertFact(s)

    def retract(self, s):
        if isinstance(s, str):
            self._knowledge_base.retractS(s)
        else:
            assert(isinstance(s[0], Node))
            self._knowledge_base.retractFact(s)
        

    def query(self, s):
        if isinstance(s, str):
            return self._knowledge_base.queryS(s)
        else:
            assert(isinstance(s, Query))
            return self._knowledge_base.queryFact(s)

    def registerRules(self, s):
        #todo: rules are strings in the factbase too
        if isinstance(s, str):
            rules = RP.parseString(s)
        else:
            assert(all([isinstance(x, Rule) for x in s]))
            rules = s
        for x in rules:
            try:
                assert(isinstance(x, Rule))
                assert(x.is_coherent())
                logging.info("Registering Rule: {}".format(x._name))
                ruleName = "".join([str(x) for x in x._name])
                self._rules[ruleName] = x
                #todo: assert the fact here
            except Exception as e:
                logging.exception(e)

