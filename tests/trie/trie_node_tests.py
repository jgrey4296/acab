import unittest
import logging
from test_context import py_rule
from py_rule.trie.nodes.trie_node import TrieNode

class (unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_node_creation(self):

    def test_add_child(self):

    def test_get_child(self):

    def test_has_child(self):

    def test_remove_child(self):

    def test_clear_children(self):

    def test_bool(self):

    def test_len(self):

    def test_contains(self):

    def test_iter(self):




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
