import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.data.trie import Trie
from acab.abstract.data.node import AcabNode
from acab.abstract.core.sentence import Sentence


class TrieTests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_initial_test(self):
        t = Trie()
        self.assertIsNotNone(t)
        self.assertIsNotNone(t._root)

    def test_empty(self):
        t = Trie()
        self.assertEqual(0, len(t))
        self.assertFalse(bool(t))

    def test_trie_add(self):
        t = Trie()
        self.assertEqual(0, len(t))
        t.add(Sentence.build(['a']))
        self.assertEqual(1, len(t))
        t.add(Sentence.build(['b']))
        self.assertEqual(2, len(t))

    def test_trie_query(self):
        t = Trie()
        self.assertEqual(0, len(t))
        t.add(Sentence.build(['a','b','c']))
        self.assertEqual(3, len(t))
        result = t.query(Sentence.build(['a','b','c']))[0]
        self.assertIsInstance(result, AcabNode)
        self.assertEqual(result.name, 'c')

    def test_trie_query_fail(self):
        t = Trie()
        self.assertEqual(0, len(t))
        t.add(Sentence.build(['a','b','c']))
        self.assertEqual(3, len(t))
        result = t.query(Sentence.build(['a','b','d']))
        self.assertFalse(result)

    def test_trie_remove(self):
        t = Trie()
        self.assertEqual(0, len(t))
        t.add(Sentence.build(['a','b','c']))
        self.assertEqual(3, len(t))
        t.remove(Sentence.build(['a','b','c']))
        self.assertEqual(2, len(t))
        result = t.query(Sentence.build(['a','b','c']))
        self.assertFalse(result)

    def test_trie_get_nodes(self):
        t = Trie()
        self.assertEqual(0, len(t))
        t.add(Sentence.build(['a','b','c']))
        self.assertEqual(3, len(t))
        nodes = t.get_nodes()
        self.assertEqual(3, len(nodes))
        self.assertEqual({x.name for x in nodes}, {'a','b','c'})


    def test_trie_pattern_match(self):
        pattern_trie = Trie()
        pattern_trie.add(Sentence.build(['a','b','c']))
        pattern_trie.add(Sentence.build(['a','b','d']))

        query_trie = Trie()
        query_trie.add(Sentence.build(['q','b','c']))
        query_trie.add(Sentence.build(['q','b','d']))
        query_trie.add(Sentence.build(['q','c','d']))

        matches = pattern_trie.match_as_pattern(query_trie, pattern_match_fn)

        self.assertEqual(len(matches), 2)


def pattern_match_fn(head, available):
    if head.name == "a":
        return available

    return [x for x in available if x.name == head.name]


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
