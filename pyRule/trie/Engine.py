"""
The combined engine of underlying trie based knowledge store,
with support for transforms and actions
"""
import logging as root_logger
from pyRule.trie.Trie import Trie
from pyRule.trie import TransformParser as TP
from pyRule.trie import ActionParser as AP
from pyRule.trie import QueryParser as QP
from pyRule.trie import FactParser as FP
import  pyRule.utils as util

logging = root_logger.getLogger(__name__)

class Engine:
    
    def __init__(self, init=None):
        self._trie = Trie(init)
        self._prior_states = []
        self._custom_actions = {}
        
    def _save_state(self):
        self._prior_states.append(str(self._trie))

    def register_action(self, name, func):
        return None
        
    def add(self, s):
        assert(isinstance(s, str))
        self._trie.assertSMulti(s)

    def retract(self, s):
        assert(isinstance(s, str))
        self._trie.retractSMulti(s)

    def query(self, s):
        assert(isinstance(s, str))
        self._trie.queryS(s)

    def registerRules(self, s):
        #rules are strings in the factbase too
        return None

    def _run_rules(self, s):
        #for all child rules at parsed(s),
        #run the rule, run the transform, run the action
        return None

    def _run_transform(self, ctx, transform):
        return None

    def _run_action(self, ctx, action):
        #lookup custom actions
        return None
    
    def _verify_rule(self, s):
        #parse a rule, then verify that
        #query -> transform -> action bindings are aligned
        return None
