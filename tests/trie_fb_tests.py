import unittest
import logging
from test_context import pyRule
import pyRule.trie as T
import IPython


class Trie_FactBase_Tests(unittest.TestCase):
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
        result = self.trie.queryS('.a.b.c?')
        self.assertIsInstance(result, T.Contexts)
        self.assertTrue(result)
        
    def test_simplest_query_fail(self):
        """ Check that not all queries pass """
        self.trie.assertS('.a.b.c')
        result = self.trie.queryS('.q.w.e?')
        self.assertFalse(result)

    def test_retract_verified_by_query(self):
        self.trie.assertS('.a.b.c, .a.b.d')
        self.trie.retractS('.a.b')
        self.assertFalse(self.trie.queryS('.a.b.c?'))
        self.assertFalse(self.trie.queryS('.a.b.d?'))
        self.assertFalse(self.trie.queryS('.a.b?'))
        self.assertTrue(self.trie.queryS('.a?'))

    def test_retract_verified_by_query_2(self):
        self.trie.assertS('.a.b.c, .a.b.d, .a.b.e')
        self.assertEqual(len(self.trie._root._reconstruct()), 3)
        self.trie.retractS('.a.b.e, .a.b.d')
        self.assertFalse(self.trie.queryS('.a.b.e?'))
        self.assertFalse(self.trie.queryS('.a.b.d?'))
        self.assertTrue(self.trie.queryS('.a.b.c?'))
        self.assertEqual(len(self.trie._root._reconstruct()), 1)
        
    def test_bind_query(self):
        """ Check that queries can bind to a value """
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.e.r')
        result = self.trie.queryS('.$x?')
        self.assertEqual(len(result), 2)

    def test_multi_assert_parse(self):
        """ Check multiple facts can be asserted in one call """
        self.trie.assertS('.a.b.c, .d.e.f, .g.e.q')
        self.assertEqual(len(self.trie._root), 3)

    def test_bind_filter(self):
        """ Check that only matching binds pass """
        self.trie.assertS('.a.b.c, .a.d.e')
        result = self.trie.queryS('.$x.d?')
        self.assertEqual(len(result),1)

    def test_query_alpha_comp(self):
        """ Check that alpha comparisons work """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(==20)?')
        self.assertTrue(result)

    def test_query_alpha_comp_fails(self):
        """ Check that alpha comparisons can fail """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(==30)?')
        self.assertFalse(result)
        
    def test_query_alpha_comp_GT(self):
        """ Check that other comparisons from equality can be tested for """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(>10)?')
        self.assertTrue(result)

    def test_query_fail(self):
        """ Check that other comparisons can fail """
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(>30)?')
        self.assertFalse(result)

    def test_query_multi_clause(self):
        """ Check that queries can have multiple clauses """
        self.trie.assertS('.a.b.c, .d.e.f')
        result = self.trie.queryS('.a.$x?, .d.$y?')
        self.assertTrue(result)
        self.assertEqual(result._alternatives[0][0]['x'], 'b')
        self.assertEqual(result._alternatives[0][0]['y'], 'e')

    def test_query_exclusion(self):
        """ Check that queries of exclusion property work """
        self.trie.assertS('.a.b!c')
        result = self.trie.queryS('.a.b!c?')
        self.assertTrue(result)

    def test_query_exclusion_fail(self):
        """ Check that exclusion is detected and can fail """
        self.trie.assertS('.a.b.c, .a.b.d')
        result = self.trie.queryS('.a.b!c?')
        self.assertFalse(result)

    def test_query_exclusion_update(self):
        """ Check that exclusion property is updated as necessary """
        self.trie.assertS('.a.b.c')
        self.assertTrue(self.trie.queryS('.a.b!c?'))
        self.trie.assertS('.a.b.d')
        self.assertFalse(self.trie.queryS('.a.b!c?'))
        self.trie.assertS('.a.b!c')
        self.assertTrue(self.trie.queryS('.a.b!c?'))

    def test_retraction_cascade(self):
        """ Check that retracting a fact retracts any subfacts """
        self.trie.assertS('.a.b.c')
        self.trie.retractS('.a')
        result = self.trie.queryS('.$x.$y?')
        self.assertFalse(result)
        
    def test_query_multi_bind_comp(self):
        """ Check that bindings hold across clauses """
        self.trie.assertS('.a.b.20, .a.c.30, .a.d.40')
        result = self.trie.queryS('.a.c.$x?, .a.$y(!=c).$v(>$x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['x'], 30)
        self.assertEqual(result[0]['y'], 'd')
        self.assertEqual(result[0]['v'], 40)

    def test_query_multi_alts(self):
        """ Check that queries with bindings provide enumerated alternatives """
        self.trie.assertS('.a.b.20, .a.c.30, .a.d.40, .a.e.50')
        result = self.trie.queryS('.a.c.$x?, .a.$y(!=c).$v(>$x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 2)

    def test_assertion_of_strings(self):
        """ Check that double quoted strings work as fact components and can be matched against """
        self.trie.assertS('.a.b."This is a test".blah')
        result = self.trie.queryS('.a.b."This is a test".blah?')
        self.assertTrue(result)
        result = self.trie.queryS('.a.b.$x.blah?')
        self.assertTrue(result)
        self.assertEqual(result[0]['x'], "This is a test")    
        
    def test_factbase_to_strings(self):
        self.trie.assertS('.a.b.c')
        self.assertEqual(str(self.trie), '.a.b.c')

    def test_factbase_to_multi_strings(self):
        self.trie.assertS('.a.b.c, .q.e.r, .t.y!u')
        s = str(self.trie)
        self.assertTrue('.a.b.c' in s)
        self.assertTrue('.q.e.r' in s)
        self.assertTrue('.t.y!u' in s)

    def test_trie_assertion_on_creation(self):
        newTrie = T.Trie('.a.b.c, .d.e.f, .q.w.e')
        result = newTrie.queryS('.a.b.c?, .d.e.f?, .q.w.e?')
        self.assertTrue(result)
        
    def test_factbase_from_string_recovery(self):
        self.trie.assertS('.a.b.c, .q.e.r, .t.y!u')
        s = str(self.trie)
        newTrie = T.Trie(s)
        self.assertEqual(self.trie, newTrie)
        self.assertEqual(s, str(newTrie))

    def test_query_negation(self):
        self.trie.assertS('.a.b.c')
        result = self.trie.queryS('.a.b.c?')
        
        self.assertTrue(result)
        result = self.trie.queryS('~.a.b.c?')
        self.assertFalse(result)
        
        result = self.trie.queryS('~.q.w.e?')
        self.assertTrue(result)

    def test_query_multi_clause_2(self):
        self.trie.assertS('.a.b.c, .d.e.f')
        self.assertTrue(self.trie.queryS('.a.b.c?'))
        self.assertTrue(self.trie.queryS('.d.e.f?'))
        self.assertFalse(self.trie.queryS('.a.b.c?, ~.d.e.f?'))
        self.assertTrue(self.trie.queryS('.a.b.c?, .d.e.f?'))

    def test_query_exclusion_negation(self):
        self.trie.assertS('.a.b!c')
        self.assertFalse(self.trie.queryS('~.a.b!c?'))
        self.trie.assertS('.a.b.d')
        self.assertTrue(self.trie.queryS('~.a.b!c?'))

    def test_query_regex(self):
        self.trie.assertS('.a.b.cBlah')
        self.assertTrue(self.trie.queryS('.a.b.$x(~= /cBlah/)?'))
        self.assertFalse(self.trie.queryS('.a.b.$x(~= /bBlah/)?'))

    def test_query_regex_bind(self):
        self.trie.assertS('.a.b.cBlah')
        result = self.trie.queryS('.a.b.$x(~= /c(?P<y>.+)/)?')
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['y'], 'Blah')

    def test_query_regex_multi_bind(self):
        self.trie.assertS('.a.b.cBlah, .a.b.cBloo, .a.b.dAwef')
        result = self.trie.queryS('.a.b.$x(~= /c(?P<y>.+)/)?')
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
