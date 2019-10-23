import unittest
import logging
from test_context import py_rule
import py_rule.utils as util
from py_rule.engines import bdi_engine as bdi
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import IPython

class BDI_TESTS(unittest.TestCase):

    def path(self, filename):
        return join('..', 'bdi_testfiles', filename)

    def setUpAgent(self, files, rulePolicies):
        self.e = bdi.Agent("testAgent", [self.path(x) for x in files],
                           rulePolicies)

    #----------
    def test_init(self):
        self.setUpAgent(["initial_load_test.trie"],
                        [(None, None)])
        self.assertIsNotNone(self.e)

    def test_load(self):
        self.setUpAgent(["initial_load_test.trie"],
                        [("propose", util.default_action_policy)])
        self.assertEqual(self.e.num_rules(), 3)
        self.e.run()
        self.assertTrue(self.e._engine.query(".count!$x(> 9)?"))


    def test_responsive(self):
        self.setUpAgent(["responsive_test.trie"],
                        [("propose", util.default_action_policy)])
        self.e.run()



    #BDI architecture to test:
    #1) addition / retraction of beliefs
    #2) addition / retraction of rules
    #3) addition / retraction of desires
    #4) generation / retraction of intentions
    #5) addition of actions
    #6) selection of actions by intention
    #7) firing of actions
    #8) updating of beliefs from actions
    #9) Logic Cycle


if __name__ == "__main__":
      #use python $filename to use this logging setup
      LOGLEVEL = logging.DEBUG
      logFileName = "log.bdi_tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
