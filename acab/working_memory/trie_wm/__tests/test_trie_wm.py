import unittest
import logging
# https://docs.python.org/3/library/unittest.mock.html
import unittest.mock as mock

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.data.contexts import Contexts
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.engine.engine import Engine
from acab.abstract.rule.production_operator import ProductionOperator

from acab.modules.operators.query import RegMatch
from acab.working_memory.trie_wm.trie_working_memory import TrieWM


class Trie_WM_Tests(unittest.TestCase):
    """ Unit test for basic Trie working memory functionality """

    def setUp(self):
        bp = BootstrapParser()
        self.trieWM = TrieWM()
        self.trieWM.assert_parsers(bp)
        self.trieWM.query_parsers(bp)

    def tearDown(self):
        self.trieWM = None

    #--------------------
    def test_init(self):
        """ Check the trieWM object exists """
        self.assertIsNotNone(self.trieWM)
        self.assertIsInstance(self.trieWM, TrieWM)

    def test_assert(self):
        """ Check assertions work """
        self.assertEqual(len(self.trieWM._internal_trie._root), 0)
        self.trieWM.add('a.b.c')
        self.assertEqual(len(self.trieWM._internal_trie._root), 1)

    def test_retract(self):
        """ Check retractions work """
        self.trieWM.add('a.b.c')
        self.assertEqual(len(self.trieWM._internal_trie._root), 1)
        self.trieWM.add('~a')
        self.assertEqual(len(self.trieWM._internal_trie._root), 0)

    def test_multi_assert(self):
        """ Check multiple assertions work """
        self.assertEqual(len(self.trieWM._internal_trie._root), 0)
        self.trieWM.add('a.b.c')
        self.trieWM.add('q.r.t')
        self.assertEqual(len(self.trieWM._internal_trie._root), 2)

    def test_multi_retract(self):
        """ Check multiple retractions work """
        self.assertEqual(len(self.trieWM._internal_trie._root), 0)
        self.trieWM.add('a.b.c')
        self.trieWM.add('q.r.t')
        self.assertEqual(len(self.trieWM._internal_trie._root), 2)
        self.trieWM.add('~a')
        self.assertEqual(len(self.trieWM._internal_trie._root), 1)
        self.trieWM.add('~q')
        self.assertEqual(len(self.trieWM._internal_trie._root), 0)

    def test_simplest_query(self):
        """ Check the simplest query works """
        self.trieWM.add('a.b.c')
        result = self.trieWM.query('a.b.c?')
        self.assertIsInstance(result, Contexts)
        self.assertTrue(result)

    def test_simplest_query_fail(self):
        """ Check that not all queries pass """
        self.trieWM.add('a.b.c')
        result = self.trieWM.query('q.w.e?')
        self.assertFalse(result)

    def test_retract_verified_by_query(self):
        self.trieWM.add('a.b.c, a.b.d')
        self.trieWM.add('~a.b')
        self.assertFalse(self.trieWM.query('a.b.c?'))
        self.assertFalse(self.trieWM.query('a.b.d?'))
        self.assertFalse(self.trieWM.query('a.b?'))
        self.assertTrue(self.trieWM.query('a?'))

    def test_bind_query(self):
        """ Check that queries can bind to a value """
        self.trieWM.add('a.b.c')
        self.trieWM.add('q.e.r')
        result = self.trieWM.query('$x?')
        self.assertEqual(len(result), 2)
        values = [d['x'].value for d in result]
        self.assertTrue('a' in values)
        self.assertTrue('q' in values)

    def test_multi_assert_parse(self):
        """ Check multiple facts can be asserted in one call """
        self.trieWM.add('a.b.c, d.e.f, g.e.q')
        self.assertEqual(len(self.trieWM._internal_trie._root), 3)

    def test_bind_filter(self):
        """ Check that only matching binds pass """
        self.trieWM.add('a.b.c, a.d.e')
        result = self.trieWM.query('$x.d?')
        self.assertEqual(len(result),1)

    def test_query_multi_clause(self):
        """ Check that queries can have multiple clauses """
        self.trieWM.add('a.b.c, d.e.f')
        result = self.trieWM.query('a.$x?, d.$y?')
        self.assertTrue(result)
        self.assertEqual(result[0]['x'].value, 'b')
        self.assertEqual(result[0]['y'].value, 'e')

    def test_query_exclusion(self):
        """ Check that queries of exclusion property work """
        self.trieWM.add('a.b!c')
        result = self.trieWM.query('a.b!c?')
        self.assertTrue(result)

    def test_query_exclusion_fail(self):
        """ Check that exclusion is detected and can fail """
        self.trieWM.add('a.b.c, a.b.d')
        result = self.trieWM.query('a.b!c?')
        self.assertFalse(result)

    def test_query_exclusion_update_fail(self):
        """ Check that exclusion property is updated as necessary """
        # TODO: make this so you can't switch between . and ! ?
        self.trieWM.add('a.b.c')
        self.assertTrue(self.trieWM.query('a.b.c?'))
        self.trieWM.add('a.b.d')
        self.assertTrue(self.trieWM.query('a.b.c?'))
        self.assertTrue(self.trieWM.query('a.b.d?'))
        self.trieWM.add('a.b!c')
        self.assertTrue(self.trieWM.query('a.b!c?'))
        self.assertFalse(self.trieWM.query('a.b.c?'))
        self.assertFalse(self.trieWM.query('a.b.d?'))

    def test_retraction_cascade(self):
        """ Check that retracting a fact retracts any subfacts """
        self.trieWM.add('a.b.c')
        self.trieWM.add('~a')
        result = self.trieWM.query('$x.$y?')
        self.assertFalse(result)

    def test_assertion_of_strings(self):
        """ Check that double quoted strings work as fact components and can be matched against """
        self.trieWM.add('a.b."This is a test".blah')
        result = self.trieWM.query('a.b."This is a test".blah?')
        self.assertTrue(result)
        result = self.trieWM.query('a.b.$x.blah?')
        self.assertTrue(result)
        self.assertEqual(result[0]['x'].value, "This is a test")

    def test_factbase_to_strings(self):
        self.trieWM.add('a.b.c')
        self.assertEqual(str(self.trieWM), 'a.b.c')

    def test_factbase_to_multi_strings(self):
        self.trieWM.add('a.b.c, q.e.r, t.y!u')
        s = str(self.trieWM)
        self.assertTrue('a.b.c' in s)
        self.assertTrue('q.e.r' in s)
        self.assertTrue('t.y!u' in s)

    def test_trie_assertion_on_creation(self):
        newTrie = TrieWM('a.b.c, d.e.f, q.w.e')
        result = newTrie.query('a.b.c?, d.e.f?, q.w.e?')
        self.assertTrue(result)

    def test_factbase_from_string_recovery(self):
        self.trieWM.add('a.b.c, q.e.r, t.y!u')
        the_s = str(self.trieWM)
        newTrie = TrieWM(the_s)
        self.assertEqual(str(self.trieWM), str(newTrie))
        orig_set = set(the_s.split("\n"))
        reconstructed_set = set(str(newTrie).split("\n"))
        self.assertTrue(orig_set == reconstructed_set)

    def test_query_negation(self):
        self.trieWM.add('a.b.c')
        self.assertTrue(self.trieWM.query('a.b.c?'))
        self.assertFalse(self.trieWM.query('~a.b.c?'))
        self.assertTrue(self.trieWM.query('~q.w.e?'))

    def test_query_multi_clause_2(self):
        self.trieWM.add('a.b.c, d.e.f')
        self.assertTrue(self.trieWM.query('a.b.c?'))
        self.assertTrue(self.trieWM.query('d.e.f?'))
        self.assertFalse(self.trieWM.query('a.b.c?, ~d.e.f?'))
        self.assertTrue(self.trieWM.query('a.b.c?, d.e.f?'))

    def test_query_exclusion_negation(self):
        self.trieWM.add('a.b!c')
        self.assertFalse(self.trieWM.query('~a.b!c?'))
        self.trieWM.add('a.b.d')
        self.assertTrue(self.trieWM.query('~a.b!c?'))

    def test_query_regex(self):
        mock_engine = mock.create_autospec(Engine)
        mock_engine.get_operator.return_value = RegMatch()

        self.trieWM.add('a.b.cBlah')
        self.assertTrue(self.trieWM.query('a.b.$x(λoperator.query.regmatch /cBlah/)?', engine=mock_engine))
        self.assertFalse(self.trieWM.query('a.b.$x(λoperator.query.regmatch /bBlah/)?', engine=mock_engine))


    def test_query_regex_bind(self):
        mock_engine = mock.create_autospec(Engine)
        mock_engine.get_operator.return_value = RegMatch()
        self.trieWM.add('a.b.cBlah')
        result = self.trieWM.query('a.b.$x(λoperator.query.regmatch /c(?P<y>.+)/)?', engine=mock_engine)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['y'], 'Blah')

    def test_query_regex_multi_bind(self):
        mock_engine = mock.create_autospec(Engine)
        mock_engine.get_operator.return_value = RegMatch()
        self.trieWM.add('a.b.cBlah, a.b.cBloo, a.b.dAwef')
        result = self.trieWM.query('a.b.$x(λoperator.query.regmatch /c(?P<y>.+)/)?', engine=mock_engine)
        self.assertEqual(len(result), 2)
        boundSet = set([x['y'] for x in result])
        self.assertTrue('Blah' in boundSet)
        self.assertTrue('Bloo' in boundSet)

    def test_sub_binding_query(self):
        self.trieWM.add('a.b.c.d')
        result = self.trieWM.query('a.$x?, @x.c.$y?')

        self.assertTrue('x' in result[0])
        self.assertTrue('y' in result[0])
        self.assertEqual(result[0]['x'].value, 'b')
        self.assertEqual(result[0]['y'].value, 'd')

    def test_multiple_sub_binding_query(self):
        self.trieWM.add('a.b.c.d')
        result = self.trieWM.query('a.$x?, @x.$y?, @y.$z?')

        self.assertTrue(all([x in result[0] for x in ['x','y','z']]))
        self.assertEqual(result[0]['x'].value, 'b')
        self.assertEqual(result[0]['y'].value, 'c')
        self.assertEqual(result[0]['z'].value, 'd')

    def test_multiple_contexts_sub_binding(self):
        self.trieWM.add('a.b.c.d')
        self.trieWM.add('a.q.c.e')
        result = self.trieWM.query('a.$x?, @x.c.$y?')
        self.assertEqual(len(result), 2)
        self.assertTrue(all([x in result[0] for x in ['x','y']]))
        self.assertTrue(all([x in result[1] for x in ['x','y']]))



if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.RuleFactTrieTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
