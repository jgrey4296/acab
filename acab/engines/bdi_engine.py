""" A BDI Based Architecture that utilizes the trie engine """
from time import sleep
import logging as root_logger
from .trie_engine import TrieEngine
import acab.util as util

logging = root_logger.getLogger(__name__)


class BDIAgent:
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

