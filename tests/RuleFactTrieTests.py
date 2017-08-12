import unittest
import logging
from test_context import pyRule
import pyRule.trie as T

class RuleFactTrieTests(unittest.TestCase):
    """ Unit test for basic Trie knowledge base functionality """

    
    def setUp(self):
        self.trie = T.Trie()

    def tearDown(self):
        self.trie = None

    def test_init(self):
        """ Check the trie object exists """
        self.assertIsNotNone(self.trie)
        self.assertIsInstance(self.trie, T.Trie)
          
    def test_assert(self):
        """ Check assertions work """
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.assertEqual(len(self.trie._root), 1)

    def test_retract(self):
        """ Check retractions work """
        self.trie.assertS('.a.b.c')
        self.assertEqual(len(self.trie._root), 1)
        self.trie.retractS('.a')
        self.assertEqual(len(self.trie._root), 0)

    def test_multi_assert(self):
        """ Check multiple assertions work """
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.r.t')
        self.assertEqual(len(self.trie._root), 2)

    def test_multi_retract(self):
        """ Check multiple retractions work """
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.r.t')
        self.assertEqual(len(self.trie._root), 2)
        self.trie.retractS('.a')
        self.assertEqual(len(self.trie._root), 1)
        self.trie.retractS('.q')
        self.assertEqual(len(self.trie._root), 0)

    def test_simplest_query(self):
        """ Check the simplest query works """
        self.trie.assertS('.a.b.c')
        result = self.trie.queryS('.a.b.c')
        self.assertIsInstance(result, T.Contexts)
        self.assertTrue(result)

    def test_simplest_query_fail(self):
        """ Check that not all queries pass """
        self.trie.assertS('.a.b.c')
        result = self.trie.queryS('.q.w.e')
        self.assertFalse(result)
        
    def test_bind_query(self):
        """ Check that queries can bind to a value """
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.e.r')
        result = self.trie.queryS('.$x')
        self.assertEqual(len(result), 2)

    def test_multi_assert_parse(self):
        """ Check multiple facts can be asserted in one call """
        self.trie.assertSMulti('.a.b.c, .d.e.f, .g.e.q')
        self.assertEqual(len(self.trie._root), 3)

    def test_bind_filter(self):
        """ Check that only matching binds pass """
        self.trie.assertSMulti('.a.b.c, .a.d.e')
        result = self.trie.queryS('.$x.d')
        self.assertEqual(len(result),1)

    def test_query_alpha_comp(self):
        """ Check that alpha comparisons work """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(==20)')
        self.assertTrue(result)

    def test_query_alpha_comp_fails(self):
        """ Check that alpha comparisons can fail """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(==30)')
        self.assertFalse(result)
        
    def test_query_alpha_comp_GT(self):
        """ Check that other comparisons from equality can be tested for """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(>10)')
        self.assertTrue(result)

    def test_query_fail(self):
        """ Check that other comparisons can fail """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(>30)')
        self.assertFalse(result)

    def test_query_multi_clause(self):
        """ Check that queries can have multiple clauses """
        self.trie.assertSMulti('.a.b.c, .d.e.f')
        result = self.trie.queryS('.a.$x, .d.$y')
        self.assertTrue(result)
        self.assertEqual(result._alternatives[0][0]['x'], 'b')
        self.assertEqual(result._alternatives[0][0]['y'], 'e')

    def test_query_exclusion(self):
        """ Check that queries of exclusion property work """
        self.trie.assertSMulti('.a.b!c')
        result = self.trie.queryS('.a.b!c')
        self.assertTrue(result)

    def test_query_exclusion_fail(self):
        """ Check that exclusion is detected and can fail """
        self.trie.assertSMulti('.a.b.c, .a.b.d')
        result = self.trie.queryS('.a.b!c')
        self.assertFalse(result)

    def test_query_exclusion_update(self):
        """ Check that exclusion property is updated as necessary """
        self.trie.assertSMulti('.a.b.c')
        self.assertTrue(self.trie.queryS('.a.b!c'))
        self.trie.assertSMulti('.a.b.d')
        self.assertFalse(self.trie.queryS('.a.b!c'))
        self.trie.assertSMulti('.a.b!c')
        self.assertTrue(self.trie.queryS('.a.b!c'))

    def test_retraction_cascade(self):
        """ Check that retracting a fact retracts any subfacts """
        self.trie.assertS('.a.b.c')
        self.trie.retractS('.a')
        result = self.trie.queryS('.$x.$y')
        self.assertFalse(result)
        
    def test_query_multi_bind_comp(self):
        """ Check that bindings hold across clauses """
        self.trie.assertSMulti('.a.b.20, .a.c.30, .a.d.40')
        result = self.trie.queryS('.a.c.$x, .a.$y(!=c).$v(>$x)')
        self.assertTrue(result)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['x'], 30)
        self.assertEqual(result[0]['y'], 'd')
        self.assertEqual(result[0]['v'], 40)

    def test_query_multi_alts(self):
        """ Check that queries with bindings provide enumerated alternatives """
        self.trie.assertSMulti('.a.b.20, .a.c.30, .a.d.40, .a.e.50')
        result = self.trie.queryS('.a.c.$x, .a.$y(!=c).$v(>$x)')
        self.assertTrue(result)
        self.assertEqual(len(result), 2)

    def test_factbase_to_strings(self):
        self.assertTrue(False)

    def test_factbase_from_string_recovery(self):
        self.assertTrue(False)

    def test_query_negation(self):
        self.assertTrue(False)

        
        
if __name__ == "__main__":
    LOGLEVEL = logging.DEBUG
    logFileName = "log.RuleFactTrieTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.getLogger().addHandler(console)
    unittest.main()
