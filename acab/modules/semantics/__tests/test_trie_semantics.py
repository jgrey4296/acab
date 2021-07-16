#!/opt/anaconda3/envs/acab/bin/python
import sys
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))
import logging
import unittest
import unittest.mock as mock
from os.path import split, splitext

import acab

config = acab.setup()

from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.node import AcabNode
from acab.abstract.core.production_abstractions import ProductionComponent
from acab.abstract.core.values import AcabValue, Sentence
from acab.modules.operators.query.query_operators import EQ
from acab.modules.semantics.context_container import (ConstraintCollection,
                                                      ContextContainer,
                                                      ContextInstance)
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.prepare("Value.Structure", "NEGATION")()
BIND_V       = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V = config.prepare("Value.Structure", "CONSTRAINT")()

class TrieSemanticTests(unittest.TestCase):
    def test_trie_insert_basic(self):
        node_sem = BasicNodeSemantics("_:node")
        trie_sem = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root.children["a"])
        self.assertTrue("sentence" in trie_struct.root.children["a"].children["test"])

    def test_trie_insert_non_exclusion(self):
        node_sem = BasicNodeSemantics("_:node")
        trie_sem = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root.children["a"])
        self.assertTrue("sentence" in trie_struct.root.children["a"].children["test"])
        self.assertTrue("other" in trie_struct.root.children["a"].children["test"])

    def test_trie_insert_exclusion(self):
        node_sem    = ExclusionNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen  = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "test", "other"])
        # Set test to be exclusive
        sen[1].data[EXOP] = EXOP_enum.EX
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root.children["a"])
        self.assertFalse("sentence" in trie_struct.root.children["a"].children["test"])
        self.assertTrue("other" in trie_struct.root.children["a"].children["test"])


    def test_trie_remove_basic(self):
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        # Negate
        neg_sen = Sentence.build(["a", "test"])
        neg_sen.data[NEGATION_V] = True

        # insert into trie
        trie_sem.insert(trie_struct, sen)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root.children["a"])
        self.assertTrue("sentence" in trie_struct.root.children["a"].children["test"])
        # remove
        trie_sem.insert(trie_struct, neg_sen)
        # verify
        self.assertTrue("a" in trie_struct.root)
        self.assertFalse("test" in trie_struct.root.children["a"])


    def test_trie_query_exact(self):
        node_sem = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # Construct context container
        ctx_container = ContextContainer.build()
        # Construct query sentence
        query_sen = Sentence.build(["a", "test", "sentence"])
        # Run query
        trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertEqual(len(ctx_container), 1)
        self.assertIsNotNone(ctx_container[0]._current)


    def test_trie_query_var(self):
        node_sem = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # Construct context container
        ctx_container = ContextContainer.build()
        # Construct query sentence
        query_sen = Sentence.build(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Run query
        trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertEqual(len(ctx_container), 2)
        result_set = {ctxInst.data['$x'] for ctxInst in ctx_container}
        self.assertEqual(result_set, {'sentence', 'other'})

    def test_trie_query_with_bind_constraints(self):
        node_sem = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "test", "test"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # Construct context container
        ctx_container = ContextContainer.build()
        # Construct query sentence
        query_sen = Sentence.build(["a", "x", "x"])
        query_sen[-2].data[BIND_V] = True
        query_sen[-1].data[BIND_V] = True
        # Run query
        trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertEqual(len(ctx_container), 1)
        self.assertEqual(ctx_container[0].data['$x'], 'test')

    def test_trie_query_with_alpha_tests(self):
        node_sem = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "blah"])
        sen2 = Sentence.build(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # Construct context container for operators
        op_loc_path = Sentence.build(["EQ"])
        operator_instance = EQ()
        op_ctx        = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_container = ContextContainer.build(op_ctx)
        # Construct query sentence
        query_sen = Sentence.build(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        the_test = ProductionComponent("alpha test",
                                       op_loc_path,
                                       [AcabValue.safe_make("blah")])
        query_sen[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertEqual(len(ctx_container), 1)
        self.assertEqual(ctx_container[0].data['$x'].name, 'blah')

    def test_trie_query_with_beta_tests(self):
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "blah"])
        sen2 = Sentence.build(["a", "different", "blah"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # Construct context container for operators
        op_loc_path = Sentence.build(["EQ"])
        operator_instance = EQ()
        op_ctx        = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_container = ContextContainer.build(op_ctx)
        # Construct query sentence
        query_sen = Sentence.build(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Second Query sentence
        query_sen2 = Sentence.build(["a", "different", "y"])
        query_sen2[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        test_var = AcabValue.safe_make("x",
                                       data={BIND_V: True})
        the_test = ProductionComponent("beta test",
                                       op_loc_path,
                                       [test_var])
        query_sen2[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        trie_sem.query(trie_struct, query_sen2, ctxs=ctx_container)
        self.assertEqual(len(ctx_container), 1)
        end_result = ctx_container.pop()
        self.assertIsInstance(end_result, ContextInstance)
        self.assertEqual(end_result.data['$y'].name, 'blah')


    def test_trie_query_with_callable_tests(self):
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "blah"])
        sen2 = Sentence.build(["a", "different", "blah"])
        # insert into trie
        trie_sem.insert(trie_struct, sen)
        trie_sem.insert(trie_struct, sen2)
        # Construct context container for operators
        op_loc_path = Sentence.build(["EQ"])
        # Note the .value's, because the operator doesn't have the unwrap decorator
        operator_instance = lambda a,b,data=None: a.value == b.value
        op_ctx        = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_container = ContextContainer.build(op_ctx)
        # Construct query sentence
        query_sen = Sentence.build(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Second Query sentence
        query_sen2 = Sentence.build(["a", "different", "y"])
        query_sen2[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        test_var = AcabValue.safe_make("x",
                                       data={BIND_V: True})
        the_test = ProductionComponent("callable test",
                                       op_loc_path,
                                       [test_var])
        query_sen2[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        trie_sem.query(trie_struct, query_sen2, ctxs=ctx_container)
        self.assertEqual(len(ctx_container), 1)
        end_result = ctx_container.pop()
        self.assertEqual(end_result.data['$y'].name, 'blah')


    # -------


    def test_trie_to_sentences_simple(self):
        # Create sem
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.insert(trie_struct, sen)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 1)
        self.assertEqual(len(results[0]), 3)

    def test_trie_to_sentences_multi(self):
        # Create sem
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.insert(trie_struct, sen)
        sen2 = Sentence.build(["a", "different", "sentence", "length"])
        trie_sem.insert(trie_struct, sen2)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 2)
        self.assertEqual(len(results[0]), 3)
        self.assertEqual(len(results[1]), 4)

    def test_trie_to_sentences_duplicates(self):
        # Create sem
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.insert(trie_struct, sen)
        sen2 = Sentence.build(["a", "test", "sentence"])
        trie_sem.insert(trie_struct, sen2)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 1)
        self.assertEqual(len(results[0]), 3)

    def test_trie_to_sentences_statements(self):
        # Create sem
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:Trie", default=(node_sem, None), handlers=[], structs=[])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.insert(trie_struct, sen)
        sen2 = Sentence.build(["a", "different", "sentence"])
        sen3 = Sentence.build(["a", "statement"])
        total_sen = sen3.attach_statement(sen2)
        trie_sem.insert(trie_struct, total_sen)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 2)
        self.assertEqual(len(results[0]), 2)
        self.assertIsInstance(results[0][-1], Sentence)






if __name__ == '__main__':
    unittest.main()
