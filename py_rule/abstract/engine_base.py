""" EngineBase: The Core Interface and implementation independent code for the
    knowledgebase engines
"""
import logging as root_logger
from .rule import Rule
from . import actions
from . import transforms
from py_rule import utils as util
logging = root_logger.getLogger(__name__)



class EngineBase:
    """ The Abstract class that wme and trie versions implement the interface of. """

    def __init__(self, kb_constructor, path=None, init=None):
        self._knowledge_base = kb_constructor(init)
        #rules categorised into tag sets?
        self._rules = {}
        self._policies = {}
        self._layers = []
        self._proposed_actions = []
        #to be updated with printed representations of the kb state after each action
        self._prior_states = []
        #named recall states of past kb states
        self._recall_states = []
        #Registered custom actions
        self._custom_actions = {}
        if path is None:
            logging.info("Not loading any files for the knowledge base")
        elif isinstance(path, list):
            for x in path:
                self.load_file(x)
        else:
            self.load_file(path)

    def load_file(self, filename):
        """ Load a file spec for the facts / rules for this engine """
        #pylint: disable=unused-argument,no-self-use
        raise Exception("Base Engine Stub")

    def register_action(self, name, func):
        """ Register custom actions,
        of the form def name(engine, paramsList) """
        assert(isinstance(name, str))
        assert(callable(func))
        if name in self._custom_actions:
            raise Exception("Duplicate action: {}".format(name))
        self._custom_actions[name] = func

    def registerRules(self, s):
        """ Register passed in rule specifications """
        #pylint: disable=unused-argument,no-self-use
        raise Exception("Base Engine Stub")

    def add(self, s):
        """ Assert a new fact into the engine """
        #pylint: disable=unused-argument,no-self-use
        raise Exception("Base Engine Stub")

    def retract(self, s):
        """ Remove a fact from the engine """
        #pylint: disable=unused-argument,no-self-use
        raise Exception("Base Engine Stub")

    def clear_proposed_actions(self):
        """ Clear the list of actions proposed by rules, but which haven't been
        enacted """
        self._proposed_actions = []

    def tick(self, inputMessages):
        """ A Single Tick of the Engine.
        Receives a list of updates from the world,
        calculates, then returns a list of output messages """
        assert(isinstance(inputMessages, list))
        #Assert input messages
        #rule the rule layers
        #return actions
        raise Exception("Abstract Method")


    def _save_state(self, data):
        """ Copy the current string representation of the knowledge base,
        and any associated data """
        self._prior_states.append((str(self._knowledge_base), data))

    def __len__(self):
        """ The number of rules in the engine """
        return len(self._rules)


    def _run_rules(self, rule_locations=None, rule_tags=None, policy=None):
        """ Run all, or some, rules of the engine, if provided a policy,
        propose actions and select from the proposals """
        self._save_state((rule_locations, rule_tags, policy, self._proposed_actions))
        rules_to_run = []
        #Get the rules:
        if rule_locations is None and rule_tags is None:
            #run all rules
            rules_to_run = list(self._rules.values())
        #otherwise, get by trie location / tag and run those
        elif rule_tags is not None:
            assert(isinstance(rule_tags, list))
            rules_to_run = [x for x in self._rules.values() \
                            if bool(x._tags.intersection(rule_tags))]
        elif rule_locations is not None:
            raise Exception('Rule Location Running is not implemented yet')

        should_propose_rules = policy is not None

        for rule in rules_to_run:
            self._run_rule(rule, propose=should_propose_rules)

        if should_propose_rules:
            self._perform_action_by_policy(policy)

    def _run_rule(self, rule, propose=False):
        """ Run an individual rule. if propose, then don't enact the results,
        merely store them for later selection """
        assert(isinstance(rule, Rule))
        assert(rule.is_coherent())
        logging.info("Running Rule: {}".format(rule._name))
        result = self.query(rule._query)
        if not bool(result):
            logging.info("Rule {} Failed".format(rule._name))
            return

        if rule._transform is None:
            selected = result.select()
        else:
            selected = result.select(rule._transform.getSelectionBounds())

        transformed = []
        for data in selected:
            transformed.append(self._run_transform(data, rule._transform))

        for data in transformed:
            self._run_actions(data, rule, propose)

    def _run_transform(self, ctx, transform):
        """ Run modifications on the bind results of a query """
        assert(isinstance(ctx, dict))
        assert(transform is None or isinstance(transform, transforms.Transform))
        chosen_ctx = ctx
        if transform is None:
            return chosen_ctx
        for x in transform.components:
            #lookup op
            opFunc = x._op
            param_length = x._num_params
            #get source
            if x.source._data[util.BIND_S]:
                source = chosen_ctx[x.source._value]
            else:
                source = x.source
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
                if x._data[util.BIND_S]:
                    bindVal = chosen_ctx[x._value]
                else:
                    bindVal = x._value
                newVal = opFunc(source, x._value, bindVal)

            #rebind or reapply
            if x.rebind is None:
                chosen_ctx[x.source.value] = newVal
            else:
                chosen_ctx[x.rebind.value] = newVal

        return chosen_ctx

    def _run_actions(self, data, ruleOrActions, propose=False):
        """ Enact, or propose, the action list or actions in a rule provided """
        assert(isinstance(data, dict))
        assert(isinstance(ruleOrActions, (Rule, list)))
        if propose:
            self._proposed_actions.append((data, ruleOrActions))
        else:
            if isinstance(ruleOrActions, Rule):
                self._perform_actions(data, ruleOrActions._actions)
            else:
                self._perform_actions(data, ruleOrActions)

    def _perform_actions(self, data, actions):
        """ Actual enaction of a set of actions """
        assert(all([isinstance(x, Actions.Action) for x in actions]))
        for x in actions:
            #lookup op
            opFunc = Actions.ACTS_LOOKUP[x._op]
            #get values from data
            values = x.get_values(data)
            #perform action op with data
            opFunc(self, values)

    def _perform_action_by_policy(self, policy):
        """ Utilize a policy to select from proposed actions,
        then perform those actions """
        logging.debug("Performing action by policy")
        assert(callable(policy))
        selected = policy(self._proposed_actions)
        assert(isinstance(selected, list))
        assert(all([isinstance(x, tuple) for x in selected]))
        for d, r in selected:
            assert(isinstance(d, dict))
            if isinstance(r, Rule):
                self._perform_actions(d, r._actions)
            else:
                self._perform_actions(d, r)


    def _register_layers(self, layers):
        raise Exception("Abstract Method")

    def _register_layer_policies(self, policies):
        raise Exception("Abstract Method")
