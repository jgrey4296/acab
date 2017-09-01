import logging as root_logger
logging = root_logger.getLogger(__name__)

from time import sleep

import pyRule.trie as T
import pyRule.utils as util

class BDI_ENGINE:
      """ A Simple Agent  """

    def __init__(self, name, startup_files,
                 rule_sequence,
                 action_policy
                 wait_time=0.5):
        logging.info("Initialising BDI: {}, {}".format(name, startup_file))
        assert(isinstance(rule_sequence, list))
        #Agent Data
        self._name = name
        self._wait_time = wait_time
        self._rule_sequence = rule_sequence
        self._action_policy = action_policy        

        #Agent Structure
        self._state_query = ".agent.{}.state.finished?".format(name)
        self._state_temp_data = ".agent.{}.state.temp.$x".format(name)
        self._state_busy_on_action = ".agent.{}.state.busy".format(name)
        self._engine = T.Engine()

        #Run data
        self._assertion_queue = []

        self.load_files(startup_files)
        #todo: register actions
        
    def load_files(self, files):
        assert(isinstance(files, list))
        for f in files:
            self._engine.load_file(f)
        
    def add_to_assertion_queue(self, string):
        assert(isinstance(s, str) or isinstance(s, list))
        self._assertion_queue.append(string)

    def _assert_queue(self):
        queue = self._assertion_queue
        while len(queue) > 0:
            next = queue.pop(0)
            self._engine.add(next)
        self._assertion_queue = []

    def _run_rule_sequence(self):
        for rule_spec in self._rule_sequence:
            self._engine.run_rules(rule_spec)

    def _select_and_perform(self):
        #if not busy or interrupt actions are specified:
        #run policy and perform
        return

    def _clear_temp_data(self):
        self._engine.retract(self._state_temp_data)
        
    def run(self):
        logging.info("Running Agent: {}".format(self._name))
        while not self._engine.query(self._state_query):
            #assert anything in the queue
            self._assert_queue()
            #run rules
            self.run_rule_sequence()
            #select action and perform
            self._select_and_perform()
            #clear temporary data
            self._clear_temp_data()
            #Wait
            sleep(self._wait_time)
        logging.info("Shutting Down Agent: {}".format(self._name))
