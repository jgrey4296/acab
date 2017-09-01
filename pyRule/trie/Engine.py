"""
The combined engine of underlying trie based knowledge store,
with support for transforms and actions
"""
import logging as root_logger
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir

from pyRule import Actions as Actions
from pyRule import Transforms
import  pyRule.utils as util
from pyRule.trie import Contexts, Trie, Rule, Node, Query
from pyRule.Actions import Action

from . import TransformParser as TP
from . import ActionParser as AP
from . import QueryParser as QP
from . import FactParser as FP
from . import RuleParser as RP
from . import FileParser as FileP

import IPython

logging = root_logger.getLogger(__name__)

class Engine:
    
    def __init__(self, init=None, path=None):
        self._trie = Trie(init)
        self._rules = {}
        #to be updated with printed representations of the trie state after each action
        self._prior_states = []
        #named recall states of past tries
        self._recall_states = []
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
        
    def _save_state(self):
        self._prior_states.append(str(self._trie))

    def register_action(self, name, func, related_facts):
        """ Register custom actions,
        of the form def(engine, paramsList) """
        assert(isinstance(name,str))
        assert(callable(func))
        if name in self._custom_actions:
            raise Exception("Duplicate action: {}".format(name))
        self._custom_actions[name]
        

    #todo: be able to assert retract or query from tries instead of strings
    def add(self, s):
        if isinstance(s, str):
            self._trie.assertS(s)
        else:
            assert(isinstance(s[0],Node))
            self._trie.assertFact(s)

    def retract(self, s):
        if isinstance(s, str):
            self._trie.retractS(s)
        else:
            assert(isinstance(s[0], Node))
            self._trie.retractFact(s)
        

    def query(self, s):
        if isinstance(s, str):
            return self._trie.queryS(s)
        else:
            return self._trie.queryFact(s)

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

    def _run_rules(self, rule_locations=None):
        #todo: make it parse the names of rules to run
        #for now, run all rules in random order
        if rule_locations is None:
            #run all rules
            for r in self._rules.values():
                self.run_rule(r)
        #otherwise, get by trie location / tag and run those

    def run_rule(self, rule):
        assert(isinstance(rule, Rule))
        assert(rule.is_coherent())
        logging.info("Running Rule: {}".format(rule._name))
        result = self.query(rule._query)
        if not bool(result):
            logging.warning("Rule {} Failed".format(rule._name))
            return

        if rule._transform is None:
            selected = result.select()
        else:
            selected = result.select(rule._transform.getSelectionBounds())

        transformed = []
        for data in selected:
            transformed.append(self._run_transform(data, rule._transform))

        for data in transformed:
            self._run_actions(data, rule._actions)
        
    
    
    def _run_transform(self, ctx, transform):
        assert(isinstance(ctx, dict))
        assert(transform is None or isinstance(transform, Transforms.Transform))
        chosen_ctx = ctx
        if transform is None:
            return chosen_ctx
        for x in transform.components:
            #lookup op
            opFunc = Transforms.TROP_LOOKUP[x.op]
            param_length = Transforms.TROP_PARAM_LENGTHS[x.op]
            #get source
            source = chosen_ctx[x.source.value]
            if param_length == 1:
                newVal = opFunc(source, chosen_ctx)
            elif param_length == 2:
                #get second param:
                if x.val is not None:
                    value = x.val
                else:
                    value = chosen_ctx[x.bind.value]
                newVal = opFunc(source, value)
            elif param_length == 3:
                if isinstance(x.bind, util.Bind):
                    bindVal = chosen_ctx[x.bind.value]
                else:
                    bindVal = x.bind
                newVal = opFunc(source, x.val, bindVal)

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
