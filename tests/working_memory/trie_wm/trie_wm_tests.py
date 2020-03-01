import unittest
import logging
from test_context import py_rule
from py_rule.working_memory.trie_wm.trie_working_memory import TrieWM
from py_rule.modules.standard_operators.operator_module import OperatorSpec
from py_rule.abstract.contexts import Contexts


class Trie_WM_Tests(unittest.TestCase):
    """ Unit test for basic Trie working memory functionality """

    @classmethod
    def setUpClass(cls):
        os = OperatorSpec()
        os._construct_comp_ops()
        os._construct_action_ops()
        os._construct_transform_ops()

    def setUp(self):
        self.trie = TrieWM()
        self.trie._build_operator_parser()

    def tearDown(self):
        self.trie = None

    def test_init(self):
        """ Check the trie object exists """
        self.assertIsNotNone(self.trie)
        self.assertIsInstance(self.trie, TrieWM)

    def test_assert(self):
        """ Check assertions work """
        self.assertEqual(len(self.trie._internal_trie._root), 0)
        self.trie.add('a.b.c')
        self.assertEqual(len(self.trie._internal_trie._root), 1)

    def test_retract(self):
        """ Check retractions work """
        self.trie.add('a.b.c')
        self.assertEqual(len(self.trie._internal_trie._root), 1)
        self.trie.retract('a')
        self.assertEqual(len(self.trie._internal_trie._root), 0)

    def test_multi_assert(self):
        """ Check multiple assertions work """
        self.assertEqual(len(self.trie._internal_trie._root), 0)
        self.trie.add('a.b.c')
        self.trie.add('q.r.t')
        self.assertEqual(len(self.trie._internal_trie._root), 2)

    def test_multi_retract(self):
        """ Check multiple retractions work """
        self.assertEqual(len(self.trie._internal_trie._root), 0)
        self.trie.add('a.b.c')
        self.trie.add('q.r.t')
        self.assertEqual(len(self.trie._internal_trie._root), 2)
        self.trie.retract('a')
        self.assertEqual(len(self.trie._internal_trie._root), 1)
        self.trie.retract('q')
        self.assertEqual(len(self.trie._internal_trie._root), 0)

    def test_simplest_query(self):
        """ Check the simplest query works """
        self.trie.add('a.b.c')
        result = self.trie.query('a.b.c?')
        self.assertIsInstance(result, Contexts)
        self.assertTrue(result)

    def test_simplest_query_fail(self):
        """ Check that not all queries pass """
        self.trie.add('a.b.c')
        result = self.trie.query('q.w.e?')
        self.assertFalse(result)

    def test_retract_verified_by_query(self):
        self.trie.add('a.b.c, a.b.d')
        self.trie.retract('a.b')
        self.assertFalse(self.trie.query('a.b.c?'))
        self.assertFalse(self.trie.query('a.b.d?'))
        self.assertFalse(self.trie.query('a.b?'))
        self.assertTrue(self.trie.query('a?'))

    def test_bind_query(self):
        """ Check that queries can bind to a value """
        self.trie.add('a.b.c')
        self.trie.add('q.e.r')
        result = self.trie.query('$x?')
        self.assertEqual(len(result), 2)
        values = [d['x'] for d in result]
        self.assertTrue('a' in values)
        self.assertTrue('q' in values)

    def test_multi_assert_parse(self):
        """ Check multiple facts can be asserted in one call """
        self.trie.add('a.b.c, d.e.f, g.e.q')
        self.assertEqual(len(self.trie._internal_trie._root), 3)

    def test_bind_filter(self):
        """ Check that only matching binds pass """
        self.trie.add('a.b.c, a.d.e')
        result = self.trie.query('$x.d?')
        self.assertEqual(len(result),1)

    def test_query_multi_clause(self):
        """ Check that queries can have multiple clauses """
        self.trie.add('a.b.c, d.e.f')
        result = self.trie.query('a.$x?, d.$y?')
        self.assertTrue(result)
        self.assertEqual(result._matches[0][0]['x'], 'b')
        self.assertEqual(result._matches[0][0]['y'], 'e')

    def test_query_exclusion(self):
        """ Check that queries of exclusion property work """
        self.trie.add('a.b!c')
        result = self.trie.query('a.b!c?')
        self.assertTrue(result)

    def test_query_exclusion_fail(self):
        """ Check that exclusion is detected and can fail """
        self.trie.add('a.b.c, a.b.d')
        result = self.trie.query('a.b!c?')
        self.assertFalse(result)

    def test_query_exclusion_update_fail(self):
        """ Check that exclusion property is updated as necessary """
        # TODO: make this so you can't switch between . and ! ?
        self.trie.add('a.b.c')
        self.assertTrue(self.trie.query('a.b.c?'))
        self.trie.add('a.b.d')
        self.assertTrue(self.trie.query('a.b.c?'))
        self.assertTrue(self.trie.query('a.b.d?'))
        self.trie.add('a.b!c')
        self.assertTrue(self.trie.query('a.b!c?'))
        self.assertFalse(self.trie.query('a.b.c?'))
        self.assertFalse(self.trie.query('a.b.d?'))

    def test_retraction_cascade(self):
        """ Check that retracting a fact retracts any subfacts """
        self.trie.add('a.b.c')
        self.trie.retract('a')
        result = self.trie.query('$x.$y?')
        self.assertFalse(result)

    def test_assertion_of_strings(self):
        """ Check that double quoted strings work as fact components and can be matched against """
        self.trie.add('a.b."This is a test".blah')
        result = self.trie.query('a.b."This is a test".blah?')
        self.assertTrue(result)
        result = self.trie.query('a.b.$x.blah?')
        self.assertTrue(result)
        self.assertEqual(result[0]['x'], "This is a test")

    def test_factbase_to_strings(self):
        self.trie.add('a.b.c')
        self.assertEqual(str(self.trie), 'a.b.c')

    def test_factbase_to_multi_strings(self):
        self.trie.add('a.b.c, q.e.r, t.y!u')
        s = str(self.trie)
        self.assertTrue('a.b.c' in s)
        self.assertTrue('q.e.r' in s)
        self.assertTrue('t.y!u' in s)

    def test_trie_assertion_on_creation(self):
        newTrie = TrieWM('a.b.c, d.e.f, q.w.e')
        result = newTrie.query('a.b.c?, d.e.f?, q.w.e?')
        self.assertTrue(result)

    def test_factbase_from_string_recovery(self):
        self.trie.add('a.b.c, q.e.r, t.y!u')
        s = str(self.trie)
        newTrie = TrieWM(s)
        self.assertEqual(self.trie, newTrie)
        orig_set = set(s.split("\n"))
        reconstructed_set = set(str(newTrie).split("\n"))
        self.assertTrue(orig_set == reconstructed_set)

    def test_query_negation(self):
        self.trie.add('a.b.c')
        self.assertTrue(self.trie.query('a.b.c?'))
        self.assertFalse(self.trie.query('~a.b.c?'))
        self.assertTrue(self.trie.query('~q.w.e?'))

    def test_query_multi_clause_2(self):
        self.trie.add('a.b.c, d.e.f')
        self.assertTrue(self.trie.query('a.b.c?'))
        self.assertTrue(self.trie.query('d.e.f?'))
        self.assertFalse(self.trie.query('a.b.c?, ~d.e.f?'))
        self.assertTrue(self.trie.query('a.b.c?, d.e.f?'))

    def test_query_exclusion_negation(self):
        self.trie.add('a.b!c')
        self.assertFalse(self.trie.query('~a.b!c?'))
        self.trie.add('a.b.d')
        self.assertTrue(self.trie.query('~a.b!c?'))

    def test_query_regex(self):
        self.trie.add('a.b.cBlah')
        self.assertTrue(self.trie.query('a.b.$x(~= /cBlah/)?'))
        self.assertFalse(self.trie.query('a.b.$x(~= /bBlah/)?'))

    def test_query_regex_bind(self):
        self.trie.add('a.b.cBlah')
        result = self.trie.query('a.b.$x(~= /c(?P<y>.+)/)?')
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['y'], 'Blah')

    def test_query_regex_multi_bind(self):
        self.trie.add('a.b.cBlah, a.b.cBloo, a.b.dAwef')
        result = self.trie.query('a.b.$x(~= /c(?P<y>.+)/)?')
        self.assertEqual(len(result), 2)
        boundSet = set([x['y'] for x in result])
        self.assertTrue('Blah' in boundSet)
        self.assertTrue('Bloo' in boundSet)



if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.RuleFactTrieTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
