"""
The combined engine of underlying trie based knowledge store,
with support for transforms and actions
"""
import logging as root_logger

from pyRule import Actions as Actions
from pyRule import Transforms
import  pyRule.utils as util
from pyRule.trie import Contexts
from pyRule.trie import Trie

from . import TransformParser as TP
from . import ActionParser as AP
from . import QueryParser as QP
from . import FactParser as FP
from . import RuleParser as RP

import IPython

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
        """ Register custom actions,
        of the form def(engine, paramsList) """
        assert(isinstance(name,str))
        assert(callable(func))
        if name in self_custom_actions:
            raise Exception("Duplicate action: {}".format(name))
        self._custom_actions[name]
        

    #todo: be able to assert retract or query from tries instead of strings
    def add(self, s):
        assert(isinstance(s, str))
        self._trie.assertS(s)

    def retract(self, s):
        assert(isinstance(s, str))
        self._trie.retractS(s)

    def query(self, s):
        assert(isinstance(s, str))
        return self._trie.queryS(s)

    def registerRules(self, s):
        #todo: rules are strings in the factbase too
        assert(isinstance(s, str))
        rules = RP.parseStrings(s)
        for x in rules:
            try:
                assert(isinstance(x, util.Rule))
                assert(x.is_coherent())
                self._rules[x._name] = x
            except Exception as e:
                logging.exception(e)

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
        assert(isinstance(transform, Transforms.Transform))
        #todo: detect min max bounds of transform
        #todo: Move this code into the transform component class?
        chosen_ctx = ctx.select()
        for x in transform.components:
            #lookup op
            opFunc = Transforms.TROP_LOOKUP[x.op]
            #get source
            source = chosen_ctx[x.source.value]
            #get second param:
            if x.val is not None:
                value = x.val
            else:
                value = chosen_ctx[x.bind.value]
                
            #perform
            newVal = opFunc(source, value)
            #rebind or reapply
            if x.rebind is None:
                chosen_ctx[x.source.value] = newVal
            else:
                chosen_ctx[x.rebind.value] = newVal

        return chosen_ctx

    def _run_actions(self, data, actions):
        assert(isinstance(data, dict))
        assert(isinstance(actions,list))
        assert(all([isinstance(x, Action) for x in actions]))
        for x in actions:
            #lookup op
            opFunc = Actions.ACTS_LOOKUP[x._op]
            #get values from data
            values = x.get_values(data)
            #perform action op with data
            opFunc(self, values)
