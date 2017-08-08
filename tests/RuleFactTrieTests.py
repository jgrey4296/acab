import unittest
import logging
from test_context import pyRule
from pyRule import FactTrie
from pyRule.TrieContexts import TrieContexts
import IPython
#https://ipython.readthedocs.io/en/stable/config/options/terminal.html
#IPython.embed(simple_prompt=True)
#in shell: ipython --simple-prompty --matplotlib

class RuleFactTrieTests(unittest.TestCase):
    
    def setUp(self):
        self.trie = FactTrie()

    def tearDown(self):
        self.trie = None

    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.trie)
        self.assertIsInstance(self.trie, FactTrie)
          
    def test_assert(self):
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.assertEqual(len(self.trie._root), 1)

    def test_retract(self):
        self.trie.assertS('.a.b.c')
        self.assertEqual(len(self.trie._root), 1)
        self.trie.retractS('.a')
        self.assertEqual(len(self.trie._root), 0)

    def test_multi_assert(self):
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.r.t')
        self.assertEqual(len(self.trie._root), 2)

    def test_multi_retract(self):
        self.assertEqual(len(self.trie._root), 0)
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.r.t')
        self.assertEqual(len(self.trie._root), 2)
        self.trie.retractS('.a')
        self.assertEqual(len(self.trie._root), 1)
        self.trie.retractS('.q')
        self.assertEqual(len(self.trie._root), 0)

    def test_simplest_query(self):
        self.trie.assertS('.a.b.c')
        result = self.trie.queryS('.a.b.c')
        self.assertIsInstance(result, TrieContexts)
        self.assertTrue(result)

    def test_bind_query(self):
        self.trie.assertS('.a.b.c')
        self.trie.assertS('.q.e.r')
        result = self.trie.queryS('.$x')
        self.assertEqual(len(result), 2)

    def test_multi_assert_parse(self):
        self.trie.assertSMulti('.a.b.c, .d.e.f, .g.e.q')
        self.assertEqual(len(self.trie._root), 3)

    def test_bind_filter(self):
        self.trie.assertSMulti('.a.b.c, .a.d.e')
        result = self.trie.queryS('.$x.d')
        self.assertEqual(len(result),1)

    def test_query_alpha_comp(self):
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(==20)')
        self.assertTrue(result)

    def test_query_alpha_comp_GT(self):
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(>10)')
        self.assertTrue(result)

    def test_query_fail(self):
        self.trie.assertS('.a.b.20')
        result = self.trie.queryS('.a.b.$x(>30)')
        self.assertFalse(result)

    def test_query_multi_clause(self):
        self.trie.assertSMulti('.a.b.c, .d.e.f')
        result = self.trie.queryS('.a.$x, .d.$y')
        self.assertTrue(result)
        self.assertEqual(result._alternatives[0][0]['x'], 'b')
        self.assertEqual(result._alternatives[0][0]['y'], 'e')

    def test_query_exclusion(self):
        self.trie.assertSMulti('.a.b!c')
        result = self.trie.queryS('.a.b!c')
        self.assertTrue(result)

    def test_query_exclusion_fail(self):
        self.trie.assertSMulti('.a.b.c, .a.b.d')
        result = self.trie.queryS('.a.b!c')
        self.assertFalse(result)

    def test_query_exclusion_update(self):
        self.trie.assertSMulti('.a.b.c')
        self.assertTrue(self.trie.queryS('.a.b!c'))
        self.trie.assertSMulti('.a.b.d')
        self.assertFalse(self.trie.queryS('.a.b!c'))
        self.trie.assertSMulti('.a.b!c')
        self.assertTrue(self.trie.queryS('.a.b!c'))

    def test_query_multi_bind_comp(self):
        self.trie.assertSMulti('.a.b.20, .a.c.30, .a.d.40')
        result = self.trie.queryS('.a.c.$x, .a.$y(!=c).$v(>$x)')
        self.assertTrue(result)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['x'], 30)
        self.assertEqual(result[0]['y'], 'd')
        self.assertEqual(result[0]['v'], 40)

    def test_query_multi_alts(self):
        self.trie.assertSMulti('.a.b.20, .a.c.30, .a.d.40, .a.e.50')
        result = self.trie.queryS('.a.c.$x, .a.$y(!=c).$v(>$x)')
        self.assertTrue(result)
        self.assertEqual(len(result), 2)
        IPython.embed(simple_prompt=True)        

        
if __name__ == "__main__":
    LOGLEVEL = logging.DEBUG
    logFileName = "log.RuleFactTrieTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.getLogger('').addHandler(console)
    unittest.main()
