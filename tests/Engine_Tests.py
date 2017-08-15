import unittest
import logging
from test_context import pyRule
import pyRule.trie as T


class (unittest.TestCase):
    
    def setUp(self):
        self.e = T.Engine()
          
        
    def tearDown(self):
        self.e = None
          
    #----------
    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.e)

        
    

      

if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.engine_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
