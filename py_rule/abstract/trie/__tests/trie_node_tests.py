import unittest
from os.path import splitext, split
import logging
from py_rule.abstract.trie.nodes.trie_node import TrieNode
from py_rule import util


class TrieNodeTests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_node_creation(self):
        the_node = TrieNode("test")
        self.assertIsNotNone(the_node)
        self.assertIsInstance(the_node, TrieNode)
        self.assertEqual(the_node._value, "test")

    def test_root(self):
        root = TrieNode.Root()
        self.assertIsInstance(root, TrieNode)
        self.assertEqual(root._value, util.ROOT_S)

    def test_split_tests(self):
        # TODO
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
