""" A BDI Based Architecture that utilizes the trie engine """
from time import sleep
import logging as root_logger
from .trie_engine import TrieEngine
import py_rule.util as util

logging = root_logger.getLogger(__name__)


class Agent:
    """ A Simple BDI Agent  """

    def __init__(self, name,
                 startup_files,
                 rule_seq_pol=None,
                 wait_time=0.5,
                 actions=None):

        if not rule_seq_pol:
            rule_seq_pol = []
        assert(isinstance(startup_files, list))
        assert(isinstance(rule_seq_pol, list))
        assert(all([isinstance(x, tuple) for x in rule_seq_pol]))
        assert(all([x[0] is None or isinstance(x[0], str) for x in rule_seq_pol]))
        assert(all([x[1] is None or callable(x[1]) for x in rule_seq_pol]))
        logging.info("Initialising BDI: {}, {}".format(name, ", ".join(startup_files)))
        # History of output
        self._history = []

        # Agent Data
        self._name = name
        self._wait_time = wait_time
        # pairings of rule sequence and selection policies:
        self._rule_seq_pols = rule_seq_pol

        # Agent Structure
        self._state_query = "~agent.{}.state.finished?".format(name)
        self._state_temp_data = "agent.{}.state.temp.$x".format(name)
        self._state_busy_on_action = "agent.{}.state.busy".format(name)
        self._engine = TrieEngine()

        # Runtime data
        self._assertion_queue = []

        # registration of custom actions
        if actions is not None:
            assert(isinstance(actions, dict))
            for k, v in actions.items():
                self._engine.register_action(k, v)

        # initialisation of startup data
        for filename in startup_files:
            self._engine.load_file(filename)

        # initialisation of agent data
        self._engine.add("agent.{}.state".format(name))

    def _assert_queue(self):
        """ Asserts each string in the queue """
        queue = self._assertion_queue
        while bool(queue):
            next_val = queue.pop(0)
            self._engine.add(next_val)
        self._assertion_queue = []

    def _run_rule_sequence(self):
        """ Runs the sequence of rule layers, with their individual selection policies, in order """
        logging.debug("Running rule sequence")
        for rule_spec, policy in self._rule_seq_pols:
            logging.debug("Running rule_spec: {} with policy: {}".format(rule_spec,
                                                                         policy))
            self._engine._run_rules(rule_tags=rule_spec.split(" "), policy=policy)

    def _clear_temp_data(self):
        """ Forces a retraction of temporary data in the working memory,
        and clears any proposed actions that were not selected for performance """
        self._engine.retract(self._state_temp_data)
        self._engine.clear_proposed_actions()

    def run(self, turnlimit=None):
        """ The main loop of the agent """
        logging.info("Running Agent: {}".format(self._name))
        turn = 0
        while self._engine.query(self._state_query):
            if turnlimit is not None and turnlimit < turn:
                break
            # assert anything in the queue
            self._assert_queue()
            # run rules
            self._run_rule_sequence()
            # clear temporary data
            self._clear_temp_data()
            # Wait
            sleep(self._wait_time)
            turn += 1
        logging.info("Shutting Down Agent: {}".format(self._name))

    def add_to_assertion_queue(self, s):
        """ Adds a fully specified string to the queue, to be asserted next tick """
        assert(isinstance(s, (str, list)))
        self._assertion_queue.append(s)

    def num_rules(self):
        return len(self._engine)
