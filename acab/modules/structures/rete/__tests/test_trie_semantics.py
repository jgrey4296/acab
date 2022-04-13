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

from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.instruction import Instruction, ProductionComponent
from acab.core.data.node import AcabNode
from acab.core.data.sentence import Sentence
from acab.core.data.value import AcabValue
from acab.interfaces.handler_system import Handler
from acab.modules.context.context_set import (ConstraintCollection,
                                              ContextInstance, ContextSet)
from acab.modules.operators.query.query_operators import EQ
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.semantics import BreadthTrieSemantics

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.prepare("Value.Structure", "NEGATION")()
BIND_V       = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V = config.prepare("Value.Structure", "CONSTRAINT")()

class TrieSemanticTests(unittest.TestCase):
    def test_trie_insert_basic(self):
        """ Check trie semantics inserts nodes in the correct places """
        node_sem = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root[sen[0]])
        self.assertTrue("sentence" in trie_struct.root[sen[:2]])

        self.assertFalse("test" in trie_struct.root)
        self.assertFalse("sentence" in trie_struct.root)
    def test_trie_insert_non_exclusion(self):
        """ Check trie insertion works without exclusion when using BasicNodeSemantics """
        node_sem = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen  = Sentence(["a", "test", "sentence"])
        sen2 = Sentence(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root[sen[0]])
        self.assertTrue("sentence" in trie_struct.root[sen[:2]])
        self.assertTrue("other" in trie_struct.root[sen[:2]])

    def test_trie_insert_exclusion(self):
        """ Check Trie insertion uses exclusion when using ExclusionNodeSemantics """
        node_sem    = ExclusionNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen  = Sentence(["a", "test", "sentence"])
        sen2 = Sentence(["a", "test", "other"])
        # Set test to be exclusive
        sen[1].data[EXOP] = EXOP_enum.EX
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root[sen[0]])
        self.assertFalse("sentence" in trie_struct.root[sen[:2]])
        self.assertTrue("other" in trie_struct.root[sen[:2]])


    def test_trie_remove_basic(self):
        """ Check trie removal of nodes """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen         = Sentence(["a", "test", "sentence"])
        # Negate
        neg_sen     = Sentence(["a", "test"])
        neg_sen.data[NEGATION_V] = True

        # insert into trie
        trie_sem.insert(sen, trie_struct)
        # check nodes are in
        self.assertTrue("a" in trie_struct.root)
        self.assertTrue("test" in trie_struct.root[sen[0]])

        self.assertTrue("sentence" in trie_struct.root[sen[:2]])
        # remove
        trie_sem.insert(neg_sen, trie_struct)
        # verify
        self.assertTrue("a" in trie_struct.root)
        self.assertFalse("test" in trie_struct.root[sen[0]])
        self.assertTrue("sentence" not in trie_struct.root[sen[:1]])

    def test_trie_query_exact(self):
        """ Check trie querying of an exact path works """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        sen2 = Sentence(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set
        ctx_set = ContextSet.build()
        # Construct query sentence
        query_sen = Sentence(["a", "test", "sentence"])
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 1)
        self.assertIsNotNone(ctx_set[0]._current)
        self.assertEqual(ctx_set[0]._current.value, "sentence")

    def test_trie_query_var(self):
        """ Check trie querying of a variable provides all applicable nodes """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        sen2 = Sentence(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set
        ctx_set = ContextSet.build()
        # Construct query sentence
        query_sen = Sentence(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 2)
        result_set = {str(ctxInst.data['x']) for ctxInst in ctx_set}
        self.assertEqual(result_set, {'sentence', 'other'})

    def test_trie_query_with_bind_constraints(self):
        """ Check trie querying respects binding constraints """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        sen2 = Sentence(["a", "test", "test"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set
        ctx_set = ContextSet.build()
        # Construct query sentence
        query_sen = Sentence(["a", "x", "x"])
        query_sen[-2].data[BIND_V] = True
        query_sen[-1].data[BIND_V] = True
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 1)
        self.assertEqual(ctx_set[0].data['x'], 'test')

    def test_trie_query_with_alpha_tests(self):
        """ Check trie quering respects alpha tests """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "blah"])
        sen2 = Sentence(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set for operators
        op_loc_path       = Sentence(["EQ"])
        operator_instance = EQ()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet.build(op_ctx)
        # Construct query sentence
        query_sen = Sentence(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        the_test = ProductionComponent("alpha test",
                                       op_loc_path,
                                       [AcabValue.build("blah")])
        query_sen[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)

        self.assertEqual(len(ctx_set), 1)
        self.assertEqual(ctx_set[0].data['x'].name, 'blah')

    def test_trie_query_with_beta_tests(self):
        """ Check trie querying respects beta tests """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "blah"])
        sen2 = Sentence(["a", "different", "blah"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set for operators
        op_loc_path       = Sentence(["EQ"])
        operator_instance = EQ()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet.build(op_ctx)
        # Construct query sentence
        query_sen = Sentence(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Second Query sentence
        query_sen2 = Sentence(["a", "different", "y"])
        query_sen2[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        test_var = AcabValue.build("x",
                                       data={BIND_V: True})
        the_test = ProductionComponent("beta test",
                                       op_loc_path,
                                       [test_var])
        query_sen2[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        trie_sem.query(query_sen2, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 1)
        end_result = ctx_set.pop()
        self.assertIsInstance(end_result, ContextInstance)
        self.assertEqual(end_result.data['y'].name, 'blah')


    def test_trie_query_with_callable_tests(self):
        """ Check trie querying respects custom callable tests """
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "blah"])
        sen2 = Sentence(["a", "different", "blah"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set for operators
        op_loc_path = Sentence(["EQ"])
        # Note the .value's, because the operator doesn't have the unwrap decorator
        operator_instance = lambda a,b,data=None: a.value == b.value
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet.build(op_ctx)
        # Construct query sentence
        query_sen = Sentence(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Second Query sentence
        query_sen2 = Sentence(["a", "different", "y"])
        query_sen2[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        test_var = AcabValue.build("x",
                                       data={BIND_V: True})
        the_test = ProductionComponent("callable test",
                                       op_loc_path,
                                       [test_var])
        query_sen2[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        trie_sem.query(query_sen2, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 1)
        end_result = ctx_set.pop()
        self.assertEqual(end_result.data['y'].name, 'blah')


    # -------
    def test_trie_negated_query(self):
        """
        a.b.c
        ~a.b.c?
        """
        ctx_set     = ContextSet.build()
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "blah"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        # Construct query sentence
        query_sen = sen[:]
        query_sen.data[NEGATION_V] = True
        # Run query
        result = trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(result), 0)

    def test_trie_negated_query2(self):
        """
        a.b
        ~a.b.c?
        """
        ctx_set     = ContextSet.build()
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "b"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        # Construct query sentence
        query_sen = Sentence(["a", "b", "c"])
        query_sen.data[NEGATION_V] = True
        # Run query

        result = trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(result), 1)


    def test_trie_to_sentences_simple(self):
        """ Check trie semantics can reduce a structure to a list of sentences """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 1)
        self.assertEqual(len(results[0]), 3)

    def test_trie_to_sentences_multi(self):
        """ Check trie semantics can reduce a structure completely """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        sen2 = Sentence(["a", "different", "sentence", "length"])
        trie_sem.insert(sen2, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 2)
        self.assertEqual(len(results[0]), 3)
        self.assertEqual(len(results[1]), 4)

    def test_trie_to_sentences_duplicates(self):
        """ Check trie semantics does not duplicate on reduction to sentences """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        sen2 = Sentence(["a", "test", "sentence"])
        trie_sem.insert(sen2, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 1)
        self.assertEqual(len(results[0]), 3)

    def test_trie_to_sentences_statements(self):
        """ Check trie semantics only puts statements as leaves in reduction """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler("node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)]) trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        sen2 = Sentence(["a", "different", "sentence"])
        sen3 = Sentence(["a", "statement"])
        total_sen = sen3.attach_statement(sen2)
        trie_sem.insert(total_sen, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 2)
        self.assertEqual(len(results[0]), 2)
        self.assertIsInstance(results[0][-1], Sentence)






if __name__ == '__main__':
    unittest.main()
