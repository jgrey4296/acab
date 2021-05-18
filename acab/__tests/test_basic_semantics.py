#!/usr/bin/env python3
"""
Test the basic stack of semantics:
Independent
Dependent
Abstraction
System
Component
"""
import logging
import unittest
import unittest.mock as mock
from os.path import split, splitext

import acab
config = acab.setup()

import acab.modules.semantics.abstractions as ASem
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.node import AcabNode
from acab.abstract.core.production_abstractions import (ActionOperator,
                                                        ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.interfaces.semantic_interfaces import (AbstractionSemantics,
                                                          SemanticSystem)
from acab.error.acab_base_exception import AcabBaseException
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag
from acab.modules.operators.transform.transform_operators import RegexOp
from acab.modules.semantics.context_container import (ConstraintCollection,
                                                      ContextContainer,
                                                      ContextInstance)
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
from acab.modules.semantics.system import BasicSemanticSystem
from acab.modules.semantics.util import SemanticOperatorWrapDecorator

EXOP         = config.value("MODAL", "exop")
EXOP_enum    = config.modal_enums[EXOP]

NEGATION_V   = config.value("Value.Structure", "NEGATION")
BIND_V       = config.value("Value.Structure", "BIND")
CONSTRAINT_V = config.value("Value.Structure", "CONSTRAINT")
QUERY_V      = config.value("Parse.Structure", "QUERY")
TRANSFORM_V  = config.value("Parse.Structure", "TRANSFORM")
ACTION_V     = config.value("Parse.Structure", "ACTION")

SEMANTIC_HINT_V = config.value("Value.Structure", "SEMANTIC_HINT")

QUERY_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "QUERY")])
ACTION_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "ACTION")])
TRANSFORM_SEM_HINT = Sentence.build([config.value("SEMANTICS", "TRANSFORM")])
RULE_SEM_HINT      = Sentence.build([config.value("SEMANTICS", "RULE")])
AGENDA_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "AGENDA")])
LAYER_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "LAYER")])
PIPELINE_SEM_HINT  = Sentence.build([config.value("SEMANTICS", "PIPELINE")])

# TODO test verify

class IndependentSemanticTests(unittest.TestCase):

    # InDependent: Node/Exclusion
    # Insert/Remove/Accessible/Get/Up/Down
    ## everything equal except insert for basic v exclusion
    def test_basic_node_insert(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertTrue(second in first)
        self.assertTrue(third in first)

    def test_exclusion_node_insert(self):
        sem = ExclusionNodeSemantics()
        # create two sub nodes
        first = sem.make(AcabValue("first"), data={EXOP: EXOP_enum.EX})
        second = sem.make(AcabValue("second"))
        third = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertFalse(second in first)
        self.assertTrue(third in first)

    def test_basic_node_insert_fail(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        # insert one into the other
        sem.insert(first, second)
        with self.assertRaises(AcabSemanticException):
            sem.insert(first, second)



    def test_basic_access_all(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, None, get_all=True)
        self.assertEqual(len(accessed), 2)

    def test_basic_access_specific(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, AcabValue("second"))
        self.assertEqual(len(accessed), 1)
        self.assertEqual(accessed[0].value, "second")

    def test_exclusion_access(self):
        sem = ExclusionNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"), data={EXOP: EXOP_enum.EX})
        # insert one into the other
        sem.insert(first, second)
        accessed = sem.access(first, AcabValue("second", data={EXOP: EXOP_enum.EX}))
        self.assertEqual(len(accessed), 1)
        self.assertEqual(accessed[0].value, "second")
    def test_exclusion_access_fail(self):
        sem = ExclusionNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"), data={EXOP: EXOP_enum.DOT})
        # insert one into the other
        sem.insert(first, second)
        accessed = sem.access(first, AcabValue("second", data={EXOP: EXOP_enum.EX}))
        self.assertEqual(len(accessed), 0)


    def test_basic_access_fail(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, AcabValue("non-existent"))
        self.assertEqual(len(accessed), 0)

    def test_basic_remove(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        self.assertTrue(second in first)
        sem.remove(first, AcabValue("second"))
        self.assertFalse(second in first)

    def test_basic_remove_fail(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        self.assertTrue(second in first)
        with self.assertRaises(AcabSemanticException):
            sem.remove(first, AcabValue("fourth"))

    # Dependent  : Trie/FSM/ASP
    # Insert/Remove/Query/Trigger

class TrieSemanticTests(unittest.TestCase):
    def test_trie_insert_basic(self):
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        node_sem    = ExclusionNodeSemantics()
        trie_sem    = BreadthTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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
        node_sem = BasicNodeSemantics()
        trie_sem = BreadthTrieSemantics(base=node_sem)
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


    def test_trie_to_sentences(self):
        # Create sem

        # create struct

        # call to_sentences

        # check
        pass

class AbstractionSemanticTests(unittest.TestCase):
    def test_transform(self):
        sem         = ASem.TransformAbstraction()
        # Construct context container for operators
        op_loc_path       = Sentence.build(["Regex"])
        operator_instance = RegexOp()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_container     = ContextContainer.build(op_ctx)
        # Add a ContextInst
        init_ctx = ctx_container.pop()
        updated_ctx = init_ctx.bind_dict({
            "$x" : AcabValue.safe_make("test")
        })
        ctx_container.push(updated_ctx)
        # Build Transform
        rebind_target = AcabValue.safe_make("y", data={BIND_V: True})
        clause = ProductionComponent("transform test",
                                     op_loc_path,
                                     ["$x", "es", "ES"],
                                     rebind=rebind_target)

        transform = ProductionContainer("Test Transform Clause", [clause])
        # Run Transform on context, don't need a semantic system yet, thus None
        sem(transform, ctx_container, None)
        # Check result
        result = ctx_container.pop()
        self.assertEqual(result['$y'].value, "tESt")


    def test_action_simple(self):
        """
        Test a simple action with simple side effect
        test sem_system use later
        """
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = 2


        # Build Semantics
        sem = ASem.ActionAbstraction()
        # Context Container for operators
        op_loc_path   = Sentence.build(["action"])
        operator_instance   = TestAction()
        op_ctx        = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_container = ContextContainer.build(op_ctx)
        # Build Action
        clause = ProductionComponent("Test Action Clause",
                                     op_loc_path,
                                     [])
        action = ProductionContainer("TestAction", [clause])
        # Run action on context with semantics
        sem(action, ctx_container, None)
        # Check side effects
        self.assertEqual(side_effect_obj['a'], 2)

    def test_action_with_params(self):
        """
        Test a simple action with simple side effect
        test sem_system use later
        """
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]


        # Build Semantics
        sem = ASem.ActionAbstraction()
        # Context Container for operators
        op_loc_path   = Sentence.build(["action"])
        operator_instance   = TestAction()
        op_ctx        = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_container = ContextContainer.build(op_ctx)
        # Build Action
        clause = ProductionComponent("Test Action Clause",
                                     op_loc_path,
                                     [AcabValue.safe_make("awef")])
        action = ProductionContainer("TestAction", [])
        action.clauses.append(clause)
        # Run action on context with semantics
        sem(action, ctx_container, None)
        # Check side effects
        self.assertEqual(side_effect_obj['a'], "awef")


    def test_container(self):
        side_effect_obj = {"a" : 1}

        class TestTransform(ProductionOperator):
            @SemanticOperatorWrapDecorator
            def __call__(self, *params, data=None):
                return params[0] + "_blah"

        class TestAction(ActionOperator):
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(AbstractionSemantics):
            def __call__(self, ins, ctxCon, semSys, data=None):
                raise AcabBaseException("TestAbsSem called")

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return str(val.value)

        transform_sem = ASem.TransformAbstraction()
        action_sem    = ASem.ActionAbstraction()
        semSys        = BasicSemanticSystem(StubAbsSemantic(), None,
                                            mapping={"_:transform" : transform_sem,
                                                     "_:action"    : action_sem},
                                            key=SemHintKey)

        consem        = ASem.ContainerAbstraction()

        trans_op_loc_path  = Sentence.build(["transform"])
        action_op_loc_path = Sentence.build(["action"])
        op_ctx             = ContextInstance(data={str(trans_op_loc_path): TestTransform(),
                                                  str(action_op_loc_path): TestAction()})
        ctx_container      = ContextContainer.build(op_ctx)

        init_ctx = ctx_container.pop()
        updated_ctx = init_ctx.bind_dict({
            "$x" : AcabValue.safe_make("test")
        })
        ctx_container.push(updated_ctx)
        # Build Container
        rebind_target    = AcabValue.safe_make("y", data={BIND_V: True})
        transform_clause = ProductionComponent("transform test",
                                               trans_op_loc_path,
                                               ["$x"],
                                               rebind=rebind_target)
        action_clause    = ProductionComponent("Test Action Clause",
                                               action_op_loc_path,
                                               ["$y"])
        container        = ProductionContainer("mixed_container",
                                               [ProductionContainer("transform", [transform_clause],
                                                                    data={SEMANTIC_HINT_V: "_:transform"}),
                                                ProductionContainer("action", [action_clause],
                                                                    data={SEMANTIC_HINT_V: "_:action"})])


        # run each element of container with semantics
        consem(container, ctx_container, semSys)

        # check result
        # orig val "test" + transform "_blah" into side effect obj "a"
        self.assertEqual(side_effect_obj["a"], "test_blah")


    def test_atomic_rule(self):
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            @SemanticOperatorWrapDecorator
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(AbstractionSemantics):
            def __call__(self, ins, ctxCon, semSys, data=None):
                raise AcabBaseException("TestAbsSem called", str(ins), data)

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None

        node_sem    = BasicNodeSemantics()
        trie_sem    = BreadthTrieSemantics(base=node_sem)

        query_sem   = ASem.QueryAbstraction()
        action_sem  = ASem.ActionAbstraction()
        rule_sem    = ASem.AtomicRuleAbstraction()
        trans_sem   = ASem.TransformAbstraction()

        trie_struct = BasicNodeStruct.build_default()
        semSys      = BasicSemanticSystem(trie_sem.query, trie_struct,
                                          mapping={QUERY_SEM_HINT  : query_sem,
                                                   ACTION_SEM_HINT : action_sem,
                                                   TRANSFORM_SEM_HINT: trans_sem,
                                                   RULE_SEM_HINT   : rule_sem},
                                          key=SemHintKey)

        action_op_loc_path = Sentence.build(["action"])
        trans_op_loc_path  = Sentence.build(["regex"])
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={str(action_op_loc_path): TestAction(),
                                                   str(trans_op_loc_path): trans_instance})
        ctx_container      = ContextContainer.build(op_ctx)

        # Construct Rule
        query_sen = Sentence.build(["a", "test", "x"], data={QUERY_V : True})
        query_sen[-1].data[BIND_V] = True

        transform_sen = ProductionComponent("transform_test",
                                            trans_op_loc_path,
                                            ["$x", "sen", "SEN"],
                                            rebind=AcabValue.safe_make("y", data={BIND_V: True}))
        action_sen    = ProductionComponent("Test Action Clause",
                                            action_op_loc_path,
                                            ['$y'])

        query     = ProductionContainer("test query", [query_sen], data={SEMANTIC_HINT_V: QUERY_SEM_HINT})
        transform = ProductionContainer("test transform", [transform_sen], data={SEMANTIC_HINT_V: TRANSFORM_SEM_HINT})
        action    = ProductionContainer("test action", [action_sen], data={SEMANTIC_HINT_V: ACTION_SEM_HINT})

        the_rule  = ProductionStructure("test rule",
                                        structure={
                                            QUERY_V     : query,
                                            TRANSFORM_V : transform,
                                            ACTION_V    : action
                                        },
                                        data={SEMANTIC_HINT_V: RULE_SEM_HINT})

        # insert a sentence into the struct
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.insert(trie_struct, sen)
        # run the rule
        result = semSys(the_rule, ctxs=ctx_container)

        # Check the result for bindings
        self.assertEqual(side_effect_obj["a"], "SENtence")

    def test_proxy_rule(self):
        # build semantics

        # construct rule

        # set up struct

        # run

        # check consequences
        pass

    def test_agenda(self):
        # Build Semantics

        # Build Contexts

        # Build Agenda

        # Run contexts through agenda

        # check successful contexts
        pass


    def test_layer(self):
        # Build semantics

        # Build Struct

        # Build layer

        # Run Layer on struct

        # Check results
        pass

    def test_pipeline(self):
        # build semantics

        # build struct

        # build pipeline

        # run pipeline

        # Check results
        pass


    def test_print(self):
        # Build semantics

        # Build sentences

        # Print sentences

        # verify
        pass

    # System





class SemanticSystemTests(unittest.TestCase):

    class StubAbsSemantic(AbstractionSemantics):
        def __call__(self, ins, ctxCon, semSys, data=None):
            raise AcabBaseException("TestAbsSem called")

    def test_construction(self):
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)
        self.assertIsInstance(semsys, SemanticSystem)
        self.assertIsInstance(semsys.base, AbstractionSemantics)
        self.assertFalse(semsys.mapping)
        self.assertFalse(semsys.structs)

    def test_default_call(self):
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)
        test_sen = Sentence.build(["test"])
        with self.assertRaises(AcabBaseException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception._str, "TestAbsSem called")

    def test_retrieval(self):
        # put some semantics in semsys.mapping
        pass

    def test_failure(self):
        # put a callable in failure
        pass

    def test_hooks(self):
        pass
