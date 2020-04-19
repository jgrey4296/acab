""" EngineBase: The Core Interface and implementation independent code for the
    production systems

Engine's are the main programming interface.
You create one with a working memory, load some modules,
and can then parse and run an agent DSL pipeline.
"""
import logging as root_logger
from os.path import exists, split

from py_rule import util

from . import action
from .agenda import Agenda
from .rule import Rule
from .working_memory import WorkingMemory


logging = root_logger.getLogger(__name__)


class Engine:
    """ The Abstract class of a production system engine. """

    def __init__(self, wm_constructor, modules=None, path=None, init=None):
        assert(issubclass(wm_constructor, WorkingMemory))
        self.__kb_constructor = wm_constructor
        self._working_memory = wm_constructor(init)
        self._pipeline = None
        self._proposed_actions = []
        # to be updated with printed representations
        # of the kb state after each action
        self._prior_states = []
        # named recall states of past kb states
        self._recall_states = {}
        # modules
        self._loaded_modules = set()
        # cached bindings
        self._cached_bindings = []

        # initialise
        if modules is not None:
            self.load_modules(*modules)
        # Populate Agenda Keywords
        Agenda.construct_subclass_tree()

        self._working_memory.build_operator_parser()

        if path is None:
            logging.info("Not loading any files for the working memory")
        elif isinstance(path, list):
            for x in path:
                self.load_file(x)
        else:
            self.load_file(path)

    # Initialisation:
    def load_modules(self, *modules):
        self._working_memory.add_modules(modules)
        self._loaded_modules.update(modules)

    def load_file(self, filename):
        """ Load a file spec for the facts / rules / layers for this engine """
        raise NotImplementedError("Base Engine Stub")

    def save_file(self, filename):
        """ Dump the content of the kb to a file to reload later """
        assert(exists(split(filename)[0]))
        with open(filename, 'w') as f:
            f.write(self._working_memory.print_as_dsl())

    # Base Actions
    def add(self, s):
        """ Assert a new fact into the engine """
        # pylint: disable=unused-argument,no-self-use
        self._working_memory.add(s)

    def retract(self, s):
        """ Remove a fact from the engine """
        # pylint: disable=unused-argument,no-self-use
        raise DeprecationWarning('Use a negated add')

    def query(self, s, cache=True):
        """ As a question of the working memory """
        result = self._working_memory.query(s)
        if cache:
            self._cached_bindings = result

    def clear_proposed_actions(self):
        """ Clear the list of actions proposed by rules, but which haven't been
        enacted """
        self._proposed_actions = []

    def tick(self, input_messages):
        """ A Single Tick of the Engine.
        Receives a list of updates from the world """
        assert(isinstance(input_messages, list))
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
        """ Entry point for running a single layer completely """
        # should save_state
        # TODO add yields
        self._save_state(layer)
        # query for the rules
        active_rules = []
        for query in layer.queries():
            query_results = self.query(query)
            for ctx in query_results:
                active_rules.append(ctx[util.LAYER_QUERY_RULE_BIND_S]._data[util.RULE_S])

        for rule in active_rules:
            self._run_rule(rule)

        for agenda in layer.agendas():
            self._select_actions_by_agenda(agenda)

    def _run_rule(self, rule):
        """ Run an individual rule """
        # TODO add yields
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
            # This is *not* an unnecessary comprehension
            # because of how parse results work
            transformed = [x for x in result]

        for data in transformed:
            self._propose_actions(data, rule)

    def _propose_actions(self, data, rule):
        """ Enact, or propose, the action list
        or actions in a rule provided
        """
        assert(isinstance(data, dict))
        assert(isinstance(rule, Rule))
        self._proposed_actions.append((data, rule))

    def _select_actions_by_agenda(self, agenda):
        """ Utilize a policy to select from proposed actions,
        then perform those actions """
        logging.debug("Selecting Actions by agenda: {}".format(agenda.__class__.__name__))
        assert(isinstance(agenda, Agenda))
        # TODO run agenda queries and transforms
        # Set agenda variables from context
        selected = agenda(self._proposed_actions, self)
        assert(isinstance(selected, list))
        assert(all([isinstance(x, tuple)
                    and isinstance(x[0], dict)
                    and isinstance(x[1], Rule) for x in selected]))
        for d, r in selected:
            self._perform_actions(d, r._action)

    def _perform_actions(self, data, act_set):
        """ Actual enaction of a set of actions """
        assert(isinstance(act_set, action.Action))
        for x in act_set:
            x(self, data)

    def _register_layers(self, layers):
        raise NotImplementedError()

    def __len__(self):
        raise NotImplementedError()
