""" EngineBase: The Core Interface and implementation independent code for the
    production systems
"""
import logging as root_logger
from .rule import Rule
from .action import Action
from .transform import Transform
from .working_memory import WorkingMemory
from py_rule import util as util
from .agenda import Agenda
logging = root_logger.getLogger(__name__)


class Engine:
    """ The Abstract class of a production system engine. """

    def __init__(self, wm_constructor, modules=None, path=None, init=None):
        assert(issubclass(wm_constructor, WorkingMemory))
        self.__kb_constructor = wm_constructor

        self._working_memory = wm_constructor(init)

        self._proposed_actions = []
        # to be updated with printed representations
        # of the kb state after each action
        self._prior_states = []
        # named recall states of past kb states
        self._recall_states = {}

        if modules is not None:
            self._working_memory.add_modules(modules)

        self._working_memory.build_operator_parser()

        if path is None:
            logging.info("Not loading any files for the working memory")
        elif isinstance(path, list):
            for x in path:
                self.load_file(x)
        else:
            self.load_file(path)

        # TODO : add definitions to type checker
        # TODO : add type classes to type checker
        # TODO : add assertions to type checker
        # TODO : add rules to type checker
        # TODO : type_checker validate

    # Initialisation:
    def load_file(self, filename):
        """ Load a file spec for the facts / rules for this engine """
        # pylint: disable=unused-argument,no-self-use
        raise NotImplementedError("Base Engine Stub")

    # Base Actions
    def add(self, s):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        self._working_memory.add(s)

    def retract(self, s):
        """ Remove a fact from the engine """
        # pylint: disable=unused-argument,no-self-use
        self._working_memory.retract(s)

    def query(self, s):
        """ As a question of the working memory """
        return self._working_memory.query(s)

    def clear_proposed_actions(self):
        """ Clear the list of actions proposed by rules, but which haven't been
        enacted """
        self._proposed_actions = []

    def tick(self, inputMessages):
        """ A Single Tick of the Engine.
        Receives a list of updates from the world """
        assert(isinstance(inputMessages, list))
        # Assert input messages
        # rule the rule layers
        # return actions
        raise NotImplementedError()

    # Export
    def _save_state(self, data):
        """ Copy the current string representation of the working memory,
        and any associated data """
        self._prior_states.append((str(self._working_memory), data))

    # Utility
    def run_layer(self, layer):
        # should save_state
        self._save_state(layer)
        # query for the rules
        # TODO: get the rules out of the result contexts
        active_rules = []
        for query in layer.queries():
            active_rules += self.query(query)

        for rule in active_rules:
            self._run_rule(rule)

        for agenda in layer.agendas():
            self._select_actions_by_agenda(agenda)

        self._perform_selected_actions()

    def _run_rule(self, rule):
        """ Run an individual rule """
        assert(isinstance(rule, Rule))
        assert(rule.is_coherent())
        logging.info("Running Rule: {}".format(rule._name))
        result = self.query(rule._query)
        if not bool(result):
            logging.info("Rule {} Failed".format(rule._name))
            return

        transformed = []
        if rule._transform:
            for data in result:
                transformed.append(rule._transform(data))
        else:
            transformed = result[:]

        for data in transformed:
            self._propose_actions(data, rule)

    def _propose_actions(self, data, ruleOrActions):
        """ Enact, or propose, the action list
        or actions in a rule provided
        """
        assert(isinstance(data, dict))
        assert(isinstance(ruleOrActions, (Rule, list)))
        self._proposed_actions.append((data, ruleOrActions))

    def _select_actions_by_agenda(self, agenda):
        """ Utilize a policy to select from proposed actions,
        then perform those actions """
        logging.debug("Performing action by policy")
        assert(isinstance(agenda, Agenda))
        selected = agenda(self._proposed_actions, self)
        assert(isinstance(selected, list))
        assert(all([isinstance(x, tuple)
                    and isinstance(x[0], dict)
                    and isinstance(x[1], Rule) for x in selected]))
        for d, r in selected:
            self._perform_actions(d, r._actions)

    def _perform_actions(self, data, actions):
        """ Actual enaction of a set of actions """
        assert(all([isinstance(x, Action) for x in actions]))
        for x in actions:
            x(self, data)

    def _register_layers(self, layers):
        raise NotImplementedError()

    def __len__(self):
        raise NotImplementedError()
