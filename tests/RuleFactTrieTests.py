import unittest
import logging
from test_context import pyRule
from pyRule import FactTrie


class RuleFactTrieTests(unittest.TestCase):
    
    def setUp(self):
        self.trie = FactTrie()

    def tearDown(self):
        self.trie = None

    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.trie)
        self.assertIsInstance(self.trie, FactTrie)
          
    def test_assert(self):
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.assertEqual(len(self.trie._root), 1)

    def test_retract(self):
        self.trie.assertS('.a.b.c')
        self.assertEqual(len(self.trie._root), 1)
        self.trie.retractS('.a')
        self.assertEqual(len(self.trie._root), 0)

    def test_multi_assert(self):
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.r.t')
        self.assertEqual(len(self.trie._root), 2)

    def test_multi_retract(self):
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.r.t')
        self.assertEqual(len(self.trie._root), 2)
        self.trie.retractS('.a')
        self.assertEqual(len(self.trie._root), 1)
        self.trie.retractS('.q')
        self.assertEqual(len(self.trie._root), 0)
      

if __name__ == "__main__":
    LOGLEVEL = logging.DEBUG
    logFileName = "log.RuleFactTrieTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.getLogger('').addHandler(console)
    unittest.main()
