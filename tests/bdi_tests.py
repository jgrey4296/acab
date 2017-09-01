import unittest
import logging
from test_context import pyRule
from pyRule import bdi_engine as bdi
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import IPython

class (unittest.TestCase):

    def path(self, filename):
        return join('.', 'bdi_testfiles', filename)
    
    def setUp(self):
        self.e = bdi.Agent()

    def tearDown(self):
        return 1

    #----------
    def test_init(self):
        self.assertIsNotNone(self.e)

    def test_load(self):
        self.e.load_file(self.path('initial_load_test.trie'))
        self.assertEqual(self.e.num_rules(), 1)

    #BDI architecture so test:
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
      LOGLEVEL = logging.INFO
      logFileName = "log."
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
