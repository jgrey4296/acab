import unittest
import logging
from test_context import py_rule
from py_rule.trie.trie import Trie


class (unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_initial_test(self):

    def test_node_spec(self):

    def test_trie_add(self):

    def test_trie_remove(self):

    def test_trie_query(self):

    def test_trie_get_nodes(self):

    def test_trie_print(self):

        


if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
