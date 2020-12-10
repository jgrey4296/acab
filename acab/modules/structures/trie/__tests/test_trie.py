import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.values import AcabValue
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import Sentence

from acab.modules.structures.trie.trie import Trie

class TrieTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_initial_test(self):
        t = Trie()
        self.assertIsNotNone(t)
        self.assertIsNotNone(t.root)

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
        result = t.query(Sentence.build(['a','b','c']))
        self.assertIsInstance(result.nodes[0], AcabNode)
        self.assertEqual(result.nodes[0].name, 'c')

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

    def test_trie_get_nodes(self):
        t = Trie()
        self.assertEqual(0, len(t))
        t.add(Sentence.build(['a','b','c']))
        self.assertEqual(3, len(t))
        nodes = t.get_nodes()
        self.assertEqual(3, len(nodes))
        self.assertEqual({x.name for x in nodes}, {'a','b','c'})

    # TODO test get_nodes with predicate and explore funcs

    def test_trie_pattern_match(self):
        pattern_trie = Trie()
        pattern_trie.add(Sentence.build(['a','b','c']))
        pattern_trie.add(Sentence.build(['a','b','d']))

        query_trie = Trie()
        query_trie.add(Sentence.build(['q','b','c']))
        query_trie.add(Sentence.build(['q','b','d']))
        query_trie.add(Sentence.build(['q','c','d']))

        matches = pattern_trie.filter_candidates(query_trie, pattern_match_fn)

        self.assertEqual(len(matches), 2)


def pattern_match_fn(head, available):
    if head.name == "a":
        return available

    return [x for x in available if x.name == head.name]


