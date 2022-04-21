import unittest
import logging as logmod
logging = logmod.getLogger(__name__)

from os.path import join, isfile, exists, isdir
from os.path import split, splitext, expanduser, abspath
from os import listdir
import timeit

import acab
acab.setup()

from acab.core.value.sentence import Sentence
from acab.core.value.instruction import ProductionStructure
from acab.core.engine.engine import Engine

def S(*words):
    return Sentence.build(words)

class Engine_Logic_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

    def path(self, filename):
        """ Navigate from the file,
        not the cwd """
        return abspath(join("testfiles", filename))

    def setUp(self):
        self.e = Engine(modules=["acab.modules.operators.standard_operators"])
        self.e.alias_module(S("acab", "modules", "operators", "standard", "operators"), S("S"))
        self.e.build()

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_simple_logic(self):
        self.e.add('a.test.rule: (::ρ)\na.b.c?\n\nλS.ActionAdd a.b.d\n\nend')
        rule = self.e.query('a.test.$x?')[0]['x']
        self.assertIsInstance(rule, ProductionStructure)
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("~a.b.d?"))
        proposals = self.e(rule)
        self.assertTrue(proposals)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query("a.b.d?"))

    def test_simple_file_load(self):
        self.e.load_file(self.path("exampleRule1.trie"))
        self.assertTrue(self.e.query("a.b.c?, ~d.e.f?"))
        rule = self.e.query('example.one.$rule?')[0]['rule']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query("a.b.c?, d.e.f?"))

    def test_multi_rule_load_from_single_file(self):
        self.e.load_file(self.path("exampleRules2.trie"))
        self.assertTrue(self.e.query("a.b.c?, a.other!ex?, ~d.e.f?, ~d.e.g?"))
        # query rule
        rules = self.e.query('example.rule.$x?')
        # run_thing
        results = []
        for d in rules:
            results += self.e(d['x'])
        self.assertEqual(len(results), 2)
        # _perform_actions
        for dr in results:
            dr[1]._action(dr[0], self.e)

        self.assertTrue(self.e.query("a.b.c?, a.other!ex?, d.e.f?, d.e.g?"))

    def test_file_load_with_comments(self):
        self.e.load_file(self.path( "exampleComments.trie"))
        self.assertTrue(self.e.query("~this.should.not.assert?"))
        self.assertFalse(self.e.query("comment.rule.one?"))
        self.assertTrue(self.e.query("comment.rule.two?"))

    def test_file_load_retraction(self):
        self.e.load_file(self.path( "retractionTest.trie"))
        self.assertTrue(self.e.query("a.b.c?"))
        rule = self.e.query('retraction.$rule?')[0]['rule']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query("~a.b.c?"))

    def test_file_load_multi_clauses(self):
        self.e.load_file(self.path( "multiclause_rule.trie"))
        self.assertTrue(self.e.query("a.b.c?, a.c!d?"))
        rule = self.e.query('multi.clause.rule.$test?')[0]['test']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query("a.b.c?, a.c!d?, a.b.e?"))

    def test_file_load_string_query(self):
        self.e.load_file(self.path("string_query_test.trie"))
        self.assertTrue(self.e.query("~a.b.e?"))
        rule = self.e.query("a.string.$rule?")[0]['rule']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query("a.b.e?"))

    def test_file_load_string_assert(self):
        self.e.load_file(self.path("string_assert_test.trie"))
        self.assertTrue(self.e.query('~a.b."an asserted string"?'))
        rule = self.e.query('a.string.$rule?')[0]['rule']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query('a.b."an asserted string"?'))

    def test_file_load_string_retract(self):
        self.e.load_file(self.path("string_retract_test.trie"))
        self.assertTrue(self.e.query('a.b."a test string"?'))
        rule = self.e.query('a.string.$rule?')[0]['rule']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query('~a.b."a test string"?'))

    def test_file_load_transform(self):
        self.e.load_file(self.path("transform_test.trie"))
        rule = self.e.query('test.$rule?')[0]['rule']
        self.assertFalse(self.e.query('result.blAH?'))
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query('result.blAH?'))

    def test_transform_selection_single(self):
        self.e.load_file(self.path("transform_selection_single.trie"))
        self.assertTrue('a.b!a?')
        rule = self.e.query('test.$rule?')[0]['rule']
        proposals = self.e(rule)
        dr = proposals[0]
        dr[1]._action(dr[0], self.e)
        self.assertTrue(self.e.query('output."b : a"?'))

    def test_file_load_multi_transform(self):
        self.e.load_file(self.path("multi_transform_test.trie"))
        self.assertTrue(self.e.query('a.b!a?, a.c!b?, a.d!c?'))

        rule = self.e.query('test.$rule?')[0]['rule']
        proposals = self.e(rule)
        for dr in proposals:
            dr[1]._action(dr[0], self.e)

        self.assertTrue(self.e.query('output."b : a test"?, output."c : b test"?, output."d : c test"?'))

    def test_file_exclusion_update(self):
        self.e.load_file(self.path("exclusion_update_test.trie"))
        self.assertTrue(self.e.query('a.b!c?, ~a.b!b?, ~a.b!a?'))

    def test_multi_rule_proposals(self):
        self.e.load_file(self.path("multi_rule_proposals.trie"))
        self.assertTrue(self.e.query('a.b.c?'))
        ctxs = self.e.query('rule.$x?')
        results = []
        for ctx in ctxs:
            rule = ctx['x']
            results += self.e(rule)

        self.assertEqual(len(results), len(ctxs))

    @unittest.skip("TODO")
    def rule_load_with_comments(self):
        self.e.load_file(self.path("rule_with_comments.trie"))
        rule = self.e._rules['a.rule']
        self.assertEqual(len(rule._tags), 2)
        self.assertEqual(len(rule._query), 2)
        self.assertEqual(len(rule._transform), 2)
        self.assertEqual(len(rule._actions), 2)
        # query rule
        # run_thing
        # _perform_actions
        self.assertTrue(self.e.query('a.d.c?, a.d.d?, a.b!30?'))


