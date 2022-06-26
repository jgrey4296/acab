#!/opt/anaconda3/envs/acab/bin/python
"""
Tests for the naive BreadthTrieSemantics,
which *does not* handle sentence values as part of a query clause.

"""
import sys
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))
import logging as logmod
from enum import Enum
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    from acab.modules.operators.query.query_operators import (EQ, AlwaysMatch,
                                                              SimpleTypeMatch)

import acab.core.defaults.value_keys as DS
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.node import AcabNode
from acab.core.util.sentences import ProductionComponent
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.interfaces.handler_system import Handler_i
from acab.interfaces.value import ValueFactory
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import (ConstraintCollection,
                                              ContextInstance, ContextSet)
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.breadth_semantics import BreadthTrieSemantics

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()
EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, _type=Enum)()

NEGATION_V   = DS.NEGATION
BIND_V       = DS.BIND
CONSTRAINT_V = DS.CONSTRAINT

class TrieSemanticTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def test_trie_insert_basic(self):
        """ Check trie semantics inserts nodes in the correct places """
        node_sem = BasicNodeSemantics().as_handler(signal="node")
        trie_sem = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
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
        node_sem = BasicNodeSemantics().as_handler(signal="node")
        trie_sem = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen  = ValueFactory.sen(["a", "test", "sentence"])
        sen2 = ValueFactory.sen(["a", "test", "other"])
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
        node_sem    = ExclusionNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen  = ValueFactory.sen(["a", "test", "sentence"])
        sen2 = ValueFactory.sen(["a", "test", "other"])
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
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen         = ValueFactory.sen(["a", "test", "sentence"])
        # Negate
        neg_sen     = ValueFactory.sen(["a", "test"])
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
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics( init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        sen2 = ValueFactory.sen(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set
        ctx_set = ContextSet(ops={"[τ=]": SimpleTypeMatch()})
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "test", "sentence"])
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 1)
        self.assertIsNotNone(ctx_set[0]._current)
        self.assertEqual(ctx_set[0]._current.value, "sentence")

    def test_trie_query_var(self):
        """ Check trie querying of a variable provides all applicable nodes """
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        sen2 = ValueFactory.sen(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set
        ctx_set = ContextSet(ops={"[τ=]": SimpleTypeMatch()})
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 2)
        result_set = {str(ctxInst.data['x']) for ctxInst in ctx_set}
        self.assertEqual(result_set, {'sentence', 'other'})

    def test_trie_query_with_bind_constraints(self):
        """ Check trie querying respects binding constraints """
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        sen2 = ValueFactory.sen(["a", "test", "test"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set
        ctx_set = ContextSet(ops={"[τ=]": SimpleTypeMatch()})
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "x", "x"])
        query_sen[-2].data[BIND_V] = True
        query_sen[-1].data[BIND_V] = True
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(ctx_set), 1)
        self.assertEqual(ctx_set[0].data['x'], 'test')

    def test_trie_query_with_alpha_tests(self):
        """ Check trie quering respects alpha tests """
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "blah"])
        sen2 = ValueFactory.sen(["a", "test", "other"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set for operators
        op_loc_path       = ValueFactory.sen(["EQ"], data={DS.TYPE_INSTANCE: DS.OPERATOR})
        operator_instance = EQ()
        ctx_set           = ContextSet({str(op_loc_path): operator_instance})
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        the_test = ProductionComponent(op_loc_path,
                                       name="alpha test",
                                       params=[ValueFactory.value("blah")])
        query_sen[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)

        self.assertEqual(len(ctx_set), 1)
        self.assertEqual(ctx_set[0].data['x'].name, 'blah')

    def test_trie_query_with_beta_tests(self):
        """ Check trie querying respects beta tests """
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "blah"])
        sen2 = ValueFactory.sen(["a", "different", "blah"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set for operators
        op_loc_path       = ValueFactory.sen(["EQ"], data={DS.TYPE_INSTANCE: DS.OPERATOR})
        operator_instance = EQ()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet(op_ctx)
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Second Query sentence
        query_sen2 = ValueFactory.sen(["a", "different", "y"])
        query_sen2[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        test_var = ValueFactory.value("x",
                                       data={BIND_V: True})
        the_test = ProductionComponent(op_loc_path,
                                       name="beta test",
                                       params=[test_var])
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
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "blah"])
        sen2 = ValueFactory.sen(["a", "different", "blah"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        trie_sem.insert(sen2, trie_struct)
        # Construct context set for operators
        op_loc_path = ValueFactory.sen(["EQ"], data={DS.TYPE_INSTANCE: DS.OPERATOR})
        # Note the .value's, because the operator doesn't have the unwrap decorator
        operator_instance = lambda a,b,data=None: a.value == b.value
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet(op_ctx)
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        # Second Query sentence
        query_sen2 = ValueFactory.sen(["a", "different", "y"])
        query_sen2[-1].data[BIND_V] = True
        # Test for equality to "sentence"
        test_var = ValueFactory.value("x",
                                       data={BIND_V: True})
        the_test = ProductionComponent(op_loc_path,
                                       name="callable test",
                                       params=[test_var])
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
        ctx_set     = ContextSet()
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "blah"])
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
        ctx_set     = ContextSet()
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "b"])
        # insert into trie
        trie_sem.insert(sen, trie_struct)
        # Construct query sentence
        query_sen = ValueFactory.sen(["a", "b", "c"])
        query_sen.data[NEGATION_V] = True
        # Run query

        result = trie_sem.query(query_sen, trie_struct, ctxs=ctx_set)
        self.assertEqual(len(result), 1)


    def test_trie_to_sentences_simple(self):
        """ Check trie semantics can reduce a structure to a list of sentences """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 1)
        self.assertEqual(len(results[0]), 3)

    def test_trie_to_sentences_multi(self):
        """ Check trie semantics can reduce a structure completely """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        sen2 = ValueFactory.sen(["a", "different", "sentence", "length"])
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
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        sen2 = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen2, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 1)
        self.assertEqual(len(results[0]), 3)

    def test_trie_to_sentences_statements(self):
        """ Check trie semantics only puts statements as leaves in reduction """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)
        sen2 = ValueFactory.sen(["a", "different", "sentence"])
        sen3 = ValueFactory.sen(["a", "statement"])
        total_sen = sen3.attach_statement(sen2)
        trie_sem.insert(total_sen, trie_struct)
        # call to_sentences
        results = trie_sem.to_sentences(trie_struct)
        # check
        self.assertEqual(len(results), 2)
        self.assertEqual(len(results[0]), 2)
        self.assertIsInstance(results[0][-1], ValueFactory.sen_fn)

    def test_trie_query_sentence_value(self):
        """
        a.[[test.sentence]]?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        sen2.data[DS.FLATTEN] = False
        # call to_sentences
        operator_instance = SimpleTypeMatch()
        ctx_set           = ContextSet({"[τ=]": operator_instance})
        results           = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertEqual(len(results), 1)

    def test_trie_query_sentence_only_match(self):
        """
        a.[[test.sentence]]?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        trie_sem.insert(sen, trie_struct)
        # Should not match against this
        sen_dummy = ValueFactory.sen(["a", "not", "match"])
        trie_sem.insert(sen_dummy, trie_struct)

        sen2 = ValueFactory.sen(["a", Sentence(["not", "match"])])
        sen2.data[DS.FLATTEN] = False
        # call to_sentences
        ctx_set = ContextSet({"[τ=]": SimpleTypeMatch()})
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertEqual(len(results), 0)

    def test_trie_query_sentence_value_var_doesnt_bind(self):
        """
        a.[[test.$x]]?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        trie_sem.insert(sen, trie_struct)
        # Should not match against this
        sen_dummy = ValueFactory.sen(["a", "test", "match"])
        trie_sem.insert(sen_dummy, trie_struct)

        sen2 = ValueFactory.sen(["a", Sentence(["test", AcabValue("y", data={DS.BIND: True})])])
        sen2.data[DS.FLATTEN] = False
        # call to_sentences
        operator_instance = SimpleTypeMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertEqual(len(results), 0)

    @unittest.expectedFailure
    def test_trie_query_sentence_value_flatten(self):
        """
        a.[[test.sentence]]? -> a.test.sentence?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        # call to_sentences
        operator_instance = AlwaysMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertEqual(len(results), 1)

    @unittest.expectedFailure
    def test_trie_query_sentence_value_flatten_var_bind(self):
        """
        a.[[test.$x]]? -> a.test.$x?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", Sentence(["test", AcabValue("x", data={DS.BIND: True})])])
        # call to_sentences
        operator_instance = AlwaysMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertIn("x", results[0])
        self.assertEqual(results[0].x, "sentence")
        self.assertEqual(len(results), 1)



    def test_trie_query_sentence_var_bound(self):
        """
        a.$x? -> a.[[test.sentence]]?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", AcabValue("x", data={DS.BIND: True})])
        # call to_sentences
        operator_instance = AlwaysMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        top = ctx_set.pop()
        ctx_set.push(top.progress({"x": Sentence(["test", "sentence"], data={DS.FLATTEN: False})}, {}))
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertEqual(len(results), 1)


    def test_trie_query_sentence_var_bound_only_matches_correct(self):
        """
        a.$x? -> a.[[test.sentence]]?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", Sentence(["test", "sentence"])])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", AcabValue("x", data={DS.BIND: True})])
        # call to_sentences
        operator_instance = AlwaysMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        top = ctx_set.pop()
        ctx_set.push(top.progress({"x": Sentence(["test", "blah"])}, {}))
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # check
        self.assertEqual(len(results), 0)

    @unittest.expectedFailure
    def test_trie_query_sentence_var_bound_flatten(self):
        """
        a.$x? -> a.test.sentence?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", AcabValue("x", data={DS.BIND: True})])
        # call to_sentences
        operator_instance = AlwaysMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        top = ctx_set.pop()
        ctx_set.push(top.progress({"x": Sentence(["test", "sentence"])}))
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        # Manual run_delayed, because it's not going through a semantic's `call`
        results.run_delayed()
        # check
        self.assertEqual(len(results), 1)

    @unittest.expectedFailure
    def test_trie_query_sentence_var_bound_flatten_sub_bind(self):
        """
        a.$x? -> a.test.sentence?
        """
        # Create sem
        node_sem    = BasicNodeSemantics().as_handler(signal="node")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = ValueFactory.sen(["a", "test", "sentence"])
        trie_sem.insert(sen, trie_struct)

        sen2 = ValueFactory.sen(["a", AcabValue("x", data={DS.BIND: True})])
        # call to_sentences
        operator_instance = AlwaysMatch()
        op_ctx            = ContextInstance(data={"τ=": operator_instance})
        ctx_set           = ContextSet(op_ctx)
        top = ctx_set.pop()
        ctx_set.push(top.progress({"x": Sentence(["test", AcabValue("y", data={DS.BIND: True})])}, {}))
        results = trie_sem.query(sen2, trie_struct, ctxs=ctx_set)
        results.run_delayed()
        # check
        self.assertEqual(len(results), 1)
        self.assertIn("y", results[0])
        self.assertEqual(results[0].y, "sentence")






if __name__ == '__main__':
    unittest.main()