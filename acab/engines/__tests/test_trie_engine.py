import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

from math import isclose

from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab.abstract.core.value import AcabValue
from acab.abstract.rule.rule import Rule
from acab.abstract.data.contexts import Contexts

import acab.engines.trie_engine as T

import acab.working_memory.trie_wm.parsing.TransformParser as TP
import acab.working_memory.trie_wm.parsing.ActionParser as AP


class Engine_Tests(unittest.TestCase):

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
        self.e = T.TrieEngine()
        self.e.build_DSL()

    def tearDown(self):
        self.e = None

    #----------
    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.e)

    def test_assert(self):
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 0)
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 1)

    def test_retract(self):
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 1)
        self.e.add('~a')
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 0)

    def test_query(self):
        self.e.add('a.b.c')
        result = self.e.query('a.b.c?')
        self.assertTrue(bool(result))

    def test_query_fail(self):
        self.assertFalse(self.e.query('a.b.c?'))

    def test_query_fail_with_asserted_facts(self):
        self.e.add('a.b.c, a.b.d')
        result = self.e.query('a.b.c?, a.b.e?')
        self.assertFalse(self.e.query('a.b.c?, a.b.e?'))

    def test_query_with_binds(self):
        self.e.add('a.b.c, a.b.d, a.d.c')
        self.assertTrue(self.e.query('a.b.$x?, a.d.$x?'))

    def test_query_with_binds_fail(self):
        self.e.add('a.b.c, a.b.d, a.d.e')
        self.assertFalse(self.e.query('a.b.$x?, a.d.$x?'))

    def test_multi_assert(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._working_memory._internal_trie.get_nodes(pred=lambda x: not bool(x))), 3)
        self.assertTrue(self.e.query('a.b.c?, a.b.d?, a.b.e?'))

    def test_multi_retract(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._working_memory._internal_trie.get_nodes(pred=lambda x: not bool(x))), 3)
        self.e.add('~a.b.e, ~a.b.d')
        self.assertEqual(len(self.e._working_memory._internal_trie.get_nodes(pred=lambda x: not bool(x))), 1)

    def test_multi_clause_query(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        result = self.e.query('a.b.c?, a.b.d?, a.b.e?')
        self.assertTrue(result)

    def test_rule_assertion(self):
        self.assertFalse(self.e.query('a.test.rule?'))
        self.e.add('a.test.rule: (::ρ)\na.b.c?\n\na.b.d\nend')
        results = self.e.query('a.test.$rule?')
        self.assertTrue(results)
        self.assertIsInstance(results[0]['rule'], Rule)


    # TODO: in place of action registration, check operators are called appropriately

