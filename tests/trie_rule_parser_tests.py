import unittest
import logging
from test_context import pyRule


class Trie_Rule_Parser_Tests(unittest.TestCase):
      
      def setUp(self):
            return 1

      def tearDown(self):
            return 1

      #----------
      #use testcase snippets
      

if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Rule_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
