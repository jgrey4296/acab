"""
The combined engine of underlying trie based knowledge store,
with support for transforms and actions
"""
import logging as root_logger
from pyRule.trie.Trie import Trie
from . import TransformParser as TP
from . import ActionParser as AP
from . import QueryParser as QP
from . import FactParser as FP
from . import RuleParser as RP
from . import Contexts
import  pyRule.utils as util

logging = root_logger.getLogger(__name__)

class Engine:
    
    def __init__(self, init=None):
        self._trie = Trie(init)
        self._rules = {}
        #to be updated with printed representations of the trie state after each action
        self._prior_states = []
        #named recall states of past tries
        self._recall_states = []
        self._custom_actions = {}
        
    def _save_state(self):
        self._prior_states.append(str(self._trie))

    def register_action(self, name, func, related_facts):
        return None

    #todo: be able to assert retract or query from tries instead of strings
    def add(self, s):
        assert(isinstance(s, str))
        self._trie.assertSMulti(s)

    def retract(self, s):
        assert(isinstance(s, str))
        self._trie.retractSMulti(s)

    def query(self, s):
        assert(isinstance(s, str))
        return self._trie.queryS(s)

    def registerRules(self, s):
        #rules are strings in the factbase too
        assert(isinstance(s, str))
        rules = RP.parseStrings(s)
        for x in rules:
            assert(isinstance(x, util.Rule))
            assert(x.is_coherent())
            self._rules[x._name] = x

    def _run_rules(self):
        #todo: make it parse the names of rules to run
        #for now, run all rules in random order
        results = []
        for r in self._rules.values():
            results.append(self.run_rule(r))

    def run_rule(self, rule):
        assert(isinstance(rule, util.Rule))
        assert(rule.is_coherent())
        logging.info("Running Rule: {}".format(rule._name))
        result = self.query(rule._query)
        if not bool(result):
            logging.warning("Rule {} Failed".format(rule._name))
            return False
        transformed = self._run_transform(result, rule._transform)
        output = self._run_actions(transformed, rule._actions)
        return output
    
    
    def _run_transform(self, ctx, transform):
        assert(isinstance(ctx, Contexts))
        #todo: detect min max bounds of transform
        chosen_ctx = ctx.select()
        for x in transform.components:
            continue
            #lookup op
            #get source
            #get second source
            #perform
            #rebind or readd
        return chosen_ctx

    def _run_actions(self, data, actions):
        results = []
        for x in actions:
            continue
            #lookup op
            #get values from data
            #perform action op with data
            #push results to results
        return results
