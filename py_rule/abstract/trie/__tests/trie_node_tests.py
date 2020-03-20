import unittest
from os.path import splitext, split
import logging
from py_rule.abstract.trie.nodes.trie_node import TrieNode

class TrieNodeTests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_node_creation(self):
        return

    def test_add_child(self):
        return

    def test_get_child(self):
        return

    def test_has_child(self):
        return

    def test_remove_child(self):
        return

    def test_clear_children(self):
        return

    def test_bool(self):
        return

    def test_len(self):
        return

    def test_contains(self):
        return

    def test_iter(self):
        return


if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
