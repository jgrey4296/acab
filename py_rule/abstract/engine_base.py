""" EngineBase: The Core Interface and implementation independent code for the
    knowledgebase engines
"""
import logging as root_logger
from .rule import Rule
from .actions import Action
from .transforms import Transform
from py_rule import utils as util
logging = root_logger.getLogger(__name__)


class EngineBase:
    """ The Abstract class of a production system engine. """

    def __init__(self, kb_constructor, path=None, init=None):
        self._knowledge_base = kb_constructor(init)
        # rules categorised into tag sets?
        self._rules = {}
        self._policies = {}
        self._layers = []
        self._proposed_actions = []
        # to be updated with printed representations of the kb state after each action
        self._prior_states = []
        # named recall states of past kb states
        self._recall_states = []
        # Registered custom actions
        self._custom_actions = {}
        if path is None:
            logging.info("Not loading any files for the knowledge base")
        elif isinstance(path, list):
            for x in path:
                self.load_file(x)
        else:
            self.load_file(path)

    #Initialisation:
    def load_file(self, filename):
        """ Load a file spec for the facts / rules for this engine """
        # pylint: disable=unused-argument,no-self-use
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
        # pylint: disable=unused-argument,no-self-use
        raise Exception("Base Engine Stub")

    #Base Actions
    def add(self, s):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        raise Exception("Base Engine Stub")

    def retract(self, s):
        """ Remove a fact from the engine """
        # pylint: disable=unused-argument,no-self-use
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
        # Assert input messages
        # rule the rule layers
        # return actions
        raise Exception("Abstract Method")

    #Export
    def _save_state(self, data):
        """ Copy the current string representation of the knowledge base,
        and any associated data """
        self._prior_states.append((str(self._knowledge_base), data))

    #Utility
    def __len__(self):
        """ The number of rules in the engine """
        return len(self._rules)

    def _run_rules(self, rule_locations=None, rule_tags=None, policy=None):
        """ Run all, or some, rules of the engine, if provided a policy,
        propose actions and select from the proposals """
        self._save_state((rule_locations, rule_tags, policy, self._proposed_actions))
        rules_to_run = []
        # Get the rules:
        if rule_locations is None and rule_tags is None:
            # run all rules
            rules_to_run = list(self._rules.values())
        # otherwise, get by trie location / tag and run those
        elif rule_tags is not None:
            assert(isinstance(rule_tags, list))
            rules_to_run = [x for x in self._rules.values()
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

        transformed = []
        for data in result:
            transformed.append(self._run_transform(data, rule._transform))

        for data in transformed:
            self._run_actions(data, rule, propose)

    def _run_transform(self, ctx, transform):
        """ Run modifications on the bound results of a query """
        assert(isinstance(ctx, dict))
        assert(transform is None or isinstance(transform, Transform))
        chosen_ctx = ctx
        if transform is None:
            return chosen_ctx
        for x in transform._components:
            # lookup op
            opFunc = x._op
            param_length = opFunc._num_params
            # get params:
            params = [chosen_ctx[y._value] if y._data[util.BIND_S] else y._value for y in x._params]

            result = opFunc(*params, chosen_ctx)

            # rebind or reapply
            if x._rebind is None:
                chosen_ctx[x._params[0]._value] = result
            else:
                chosen_ctx[x._rebind._value] = result

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
            # lookup op
            opFunc = x._op
            # get values from data
            values = x.get_values(data)
            # perform action op with data
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
