#!/usr/bin/env python3
"""
Test the basic stack of semantics:
Independent
Dependent
Abstraction
System
Component
"""

# https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging
from acab.abstract.config.config import AcabConfig
config       = AcabConfig.Get("acab/abstract/config")

from acab.error.acab_semantic_exception import AcabSemanticException
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.production_abstractions import ProductionComponent
from acab.modules.semantics.independent import BasicNodeSemantics, ExclusionNodeSemantics
from acab.modules.semantics.dependent import BasicTrieSemantics
from acab.modules.semantics.context_container import ContextContainer, ContextInstance, ConstraintCollection
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag

EXOP         = config.value("MODAL", "exop")
EXOP_enum    = config.modal_enums[EXOP]

NEGATION_V   = config.value("Value.Structure", "NEGATION")
BIND         = config.value("Value.Structure", "BIND")
CONSTRAINT_V = config.value("Value.Structure", "CONSTRAINT")

class BasicSemanticTests(unittest.TestCase):

    # InDependent: Node/Exclusion
    # Insert/Remove/Accessible/Get/Up/Down
    ## everything equal except insert for basic v exclusion
    def test_basic_node_insert(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertTrue(second in first)
        self.assertTrue(third in first)

    def test_exclusion_node_insert(self):
        # create two nodes
        first = AcabNode(AcabValue("first"), data={EXOP: EXOP_enum.EX})
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = ExclusionNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertFalse(second in first)
        self.assertTrue(third in first)

    def test_basic_node_insert_fail(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        with self.assertRaises(AcabSemanticException):
            sem.insert(first, second)



    def test_basic_access_all(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, None, get_all=True)
        self.assertEqual(len(accessed), 2)

    def test_basic_access_specific(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, AcabValue("second"))
        self.assertEqual(len(accessed), 1)
        self.assertEqual(accessed[0].value, "second")

    def test_basic_access_fail(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, AcabValue("non-existent"))
        self.assertEqual(len(accessed), 0)

    def test_basic_remove(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)

        self.assertTrue(second in first)
        sem.remove(first, AcabValue("second"))
        self.assertFalse(second in first)

    def test_basic_remove_fail(self):
        # create two nodes
        first = AcabNode(AcabValue("first"))
        second = AcabNode(AcabValue("second"))
        third = AcabNode(AcabValue("third"))
        # insert one into the other
        sem = BasicNodeSemantics()
        sem.insert(first, second)
        sem.insert(first, third)

        self.assertTrue(second in first)
        with self.assertRaises(AcabSemanticException):
            sem.remove(first, AcabValue("fourth"))

    # Dependent  : Trie/FSM/ASP
    # Insert/Remove/Query/Trigger
    def test_trie_insert_basic(self):
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        node_sem = ExclusionNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
        trie_struct = BasicNodeStruct.build_default()
        # Create sentence
        sen = Sentence.build(["a", "test", "sentence"])
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
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        result = trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertIsInstance(result, ContextContainer)
        self.assertEqual(len(result), 1)
        self.assertIsNotNone(result[0]._current)


    def test_trie_query_var(self):
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        query_sen[-1].data[BIND] = True
        # Run query
        result = trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertIsInstance(result, ContextContainer)
        self.assertEqual(len(result), 2)
        result_set = {ctxInst.data['$x'] for ctxInst in result}
        self.assertEqual(result_set, {'sentence', 'other'})

    def test_trie_query_with_bind_constraints(self):
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        query_sen[-2].data[BIND] = True
        query_sen[-1].data[BIND] = True
        # Run query
        result = trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertIsInstance(result, ContextContainer)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0].data['$x'], 'test')

    def test_trie_query_with_alpha_tests(self):
        node_sem = BasicNodeSemantics()
        trie_sem = BasicTrieSemantics(base=node_sem)
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
        query_sen[-1].data[BIND] = True
        # Test for equality to "sentence"
        the_test = ProductionComponent("alpha test",
                                       op_loc_path,
                                       [AcabValue.safe_make("blah")])
        query_sen[-1].data[CONSTRAINT_V] = [the_test]
        # Run query
        result = trie_sem.query(trie_struct, query_sen, ctxs=ctx_container)
        self.assertIsInstance(result, ContextContainer)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0].data['$x'].name, 'blah')

    def test_trie_query_with_beta_tests(self):
        pass

    def test_trie_query_with_callable_tests(self):
        pass

    def test_fsm_insert(self):
        pass

    def test_fsm_trigger(self):
        pass

    # Abstraction: Rule/Agenda/Layer/Pipeline/Printer
    def test_rule_trigger(self):
        # Construct

        # Set up struct

        # run

        # test
        pass

    def test_agenda_trigger(self):
        pass

    def test_layer_trigger(self):
        pass

    def test_pipeline_trigger(self):
        pass

    def test_print_trigger(self):
        pass

    # System
    def test_system_trigger(self):
        pass

    # Component: Listener/Logger/Failure/Query

    # TODO mid-sentence semantics switch for dependent (trie -> FSM)
    # Or can semantics only switch between sentences?

    # TODO test entry/exit hooks
