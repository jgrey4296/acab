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

import acab.modules.semantics.abstractions as ASem
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.node import AcabNode
from acab.abstract.core.production_abstractions import (ActionOperator,
                                                        ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorResultWrap)
from acab.abstract.interfaces.handler_system import Handler
from acab.abstract.interfaces.semantic import (AbstractionSemantics_i,
                                               SemanticSystem_i)
from acab.error.acab_base_exception import AcabBaseException
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import (ConstraintCollection,
                                              ContextInstance, ContextSet)
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag
from acab.modules.operators.transform.transform_operators import RegexOp
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V      = config.prepare("Value.Structure", "NEGATION")()
BIND_V          = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V    = config.prepare("Value.Structure", "CONSTRAINT")()
SEMANTIC_HINT_V = config.prepare("Value.Structure", "SEMANTIC_HINT")()
QUERY_V         = config.prepare("Value.Structure", "QUERY")()

QUERY_C      = config.prepare("Structure.Components", "QUERY")()
TRANSFORM_C  = config.prepare("Structure.Components", "TRANSFORM")()
ACTION_C     = config.prepare("Structure.Components", "ACTION")()


QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])

# TODO test verify

class AbstractionSemanticTests(unittest.TestCase):
    def test_transform(self):
        sem                                 = ASem.TransformAbstraction()
        # Construct context set for operators
        op_loc_path                         = Sentence.build(["Regex"])
        operator_instance                   = RegexOp()
        op_ctx                              = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set                             = ContextSet.build(op_ctx)
        # Add a ContextInst
        init_ctx                            = ctx_set.pop()
        updated_ctx                         = init_ctx.bind_dict({
            "x" : AcabValue.safe_make("test")
        })
        ctx_set.push(updated_ctx)
        # Build Transform
        rebind_target                       = AcabValue.safe_make("y", data={BIND_V: True})
        clause                              = ProductionComponent("transform test",
                                                                  op_loc_path,
                                                                  ["x", "es", "ES"],
                                                                  rebind =rebind_target)

        transform                           = ProductionContainer("Test Transform Clause", [clause])
        # Run Transform on context, don't need a semantic system yet, thus None
        sem(transform, None, ctxs=ctx_set)
        # Check result
        result = ctx_set.pop()
        self.assertEqual(result['y'].value, "tESt")


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
        # Context Set for operators
        op_loc_path       = Sentence.build(["action"])
        operator_instance = TestAction()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet.build(op_ctx)
        # Build Action
        clause = ProductionComponent("Test Action Clause",
                                     op_loc_path,
                                     [])
        action = ProductionContainer("TestAction", [clause])
        # Run action on context with semantics
        sem(action, None, ctxs=ctx_set)
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
        # Context Set for operators
        op_loc_path       = Sentence.build(["action"])
        operator_instance = TestAction()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet.build(op_ctx)
        # Build Action
        clause = ProductionComponent("Test Action Clause",
                                     op_loc_path,
                                     [AcabValue.safe_make("awef")])
        action = ProductionContainer("TestAction", [])
        action.clauses.append(clause)
        # Run action on context with semantics
        sem(action, None, ctxs=ctx_set)
        # Check side effects
        self.assertEqual(side_effect_obj['a'], "awef")


    def test_container(self):
        side_effect_obj = {"a" : 1}

        class TestTransform(ProductionOperator):
            @OperatorArgUnWrap
            @OperatorResultWrap
            def __call__(self, *params, data=None):
                return params[0] + "_blah"

        class TestAction(ActionOperator):
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(AbstractionSemantics_i):
            def __call__(self, ins, semSys, ctxs=None, data=None):
                raise AcabBaseException("TestAbsSem called")

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return str(val.value)

        transform_sem = ASem.TransformAbstraction().as_handler("_:transform")
        action_sem    = ASem.ActionAbstraction().as_handler("_:action")
        semSys        = BasicSemanticSystem(default=StubAbsSemantic().as_handler("_:stub"),
                                            init_handlers=[transform_sem, action_sem])

        consem        = ASem.ContainerAbstraction()

        trans_op_loc_path  = Sentence.build(["transform"])
        action_op_loc_path = Sentence.build(["action"])
        op_ctx             = ContextInstance(data={str(trans_op_loc_path): TestTransform(),
                                                  str(action_op_loc_path): TestAction()})
        ctx_set      = ContextSet.build(op_ctx)

        init_ctx = ctx_set.pop()
        updated_ctx = init_ctx.bind_dict({
            "x" : AcabValue.safe_make("test")
        })
        ctx_set.push(updated_ctx)
        # Build Container
        rebind_target    = AcabValue.safe_make("y", data={BIND_V: True})
        transform_clause = ProductionComponent("transform test",
                                               trans_op_loc_path,
                                               ["x"],
                                               rebind=rebind_target)
        action_clause    = ProductionComponent("Test Action Clause",
                                               action_op_loc_path,
                                               ["y"])
        container        = ProductionContainer("mixed_container",
                                               [ProductionContainer("transform", [transform_clause],
                                                                    data={SEMANTIC_HINT_V: "_:transform"}),
                                                ProductionContainer("action", [action_clause],
                                                                    data={SEMANTIC_HINT_V: "_:action"})])


        # run each element of container with semantics
        consem(container, semSys, ctxs=ctx_set)

        # check result
        # orig val "test" + transform "_blah" into side effect obj "a"
        self.assertEqual(side_effect_obj["a"], "test_blah")


    def test_atomic_rule(self):
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            @OperatorArgUnWrap
            @OperatorResultWrap
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(AbstractionSemantics_i):
            def __call__(self, ins, semSys, ctxs=None, data=None):
                raise AcabBaseException("TestAbsSem called", rest=[str(ins), data])

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None

        # Build Semantics
        node_sem    = BasicNodeSemantics().as_handler("_:node")
        trie_sem    = BreadthTrieSemantics(default=node_sem).as_handler("_:trie",
                                                                        struct=BasicNodeStruct.build_default())

        query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
        action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
        rule_sem    = ASem.AtomicRuleAbstraction().as_handler(RULE_SEM_HINT)
        trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
        cont_sem    = ASem.ContainerAbstraction().as_handler("_:CONTAINER")

        semSys      = BasicSemanticSystem(init_handlers=[cont_sem,
                                                         query_sem,
                                                         action_sem,
                                                         rule_sem,
                                                         trans_sem],
                                          default=trie_sem)

        # Setup operators in context
        action_op_loc_path = Sentence.build(["action"])
        trans_op_loc_path  = Sentence.build(["regex"])
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={str(action_op_loc_path): TestAction(),
                                                   str(trans_op_loc_path): trans_instance})
        ctx_set            = ContextSet.build(op_ctx)

        # Construct Rule
        query_sen = Sentence.build(["a", "test", "x"], data={QUERY_V : True})
        query_sen[-1].data[BIND_V] = True

        transform_sen = ProductionComponent("transform_test",
                                            trans_op_loc_path,
                                            ["x", "sen", "SEN"],
                                            rebind=AcabValue.safe_make("y", data={BIND_V: True}))
        action_sen    = ProductionComponent("Test Action Clause",
                                            action_op_loc_path,
                                            ['y'])

        query     = ProductionContainer("test query", [query_sen], data={SEMANTIC_HINT_V: QUERY_SEM_HINT})
        transform = ProductionContainer("test transform", [transform_sen], data={SEMANTIC_HINT_V: TRANSFORM_SEM_HINT})
        action    = ProductionContainer("test action", [action_sen], data={SEMANTIC_HINT_V: ACTION_SEM_HINT})

        the_rule  = ProductionStructure("test rule",
                                        structure={
                                            QUERY_C     : query,
                                            TRANSFORM_C : transform,
                                            ACTION_C    : action
                                        },
                                        data={SEMANTIC_HINT_V: RULE_SEM_HINT})

        # insert a sentence into the struct
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.func.insert(sen, trie_sem.struct)
        # run the rule
        result = semSys(the_rule, ctxs=ctx_set)

        # Check the result for bindings
        self.assertEqual(side_effect_obj["a"], "SENtence")

    def test_proxy_rule(self):
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            @OperatorArgUnWrap
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(AbstractionSemantics_i):
            def __call__(self, ins, semSys, ctxs=None, data=None):
                raise AcabBaseException("TestAbsSem called", rest=[str(ins), data])

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None
        #
        # Build Semantics
        node_sem    = BasicNodeSemantics().as_handler("_:node")
        trie_sem    = BreadthTrieSemantics(default=node_sem).as_handler("_:trie",
                                                                        struct=BasicNodeStruct.build_default())

        query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
        action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
        trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
        # THIS IS THE MAJOR CHANGE OF THIS TEST:
        rule_sem    = ASem.ProxyRuleAbstraction().as_handler(RULE_SEM_HINT)

        semSys      = BasicSemanticSystem(init_handlers=[query_sem, action_sem, trans_sem, rule_sem],
                                          default=trie_sem)

        # Setup operators in context
        action_op_loc_path = Sentence.build(["action"])
        trans_op_loc_path  = Sentence.build(["regex"])
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={str(action_op_loc_path): TestAction(),
                                                   str(trans_op_loc_path): trans_instance})
        ctx_set            = ContextSet.build(op_ctx)

        # Construct Rule
        query_sen = Sentence.build(["a", "test", "x"], data={QUERY_V : True})
        query_sen[-1].data[BIND_V] = True

        transform_sen = ProductionComponent("transform_test",
                                            trans_op_loc_path,
                                            ["x", "sen", "SEN"],
                                            rebind=AcabValue.safe_make("y", data={BIND_V: True}))
        action_sen    = ProductionComponent("Test Action Clause",
                                            action_op_loc_path,
                                            ['y'])

        query     = ProductionContainer("test query", [query_sen], data={SEMANTIC_HINT_V: QUERY_SEM_HINT})
        transform = ProductionContainer("test transform", [transform_sen], data={SEMANTIC_HINT_V: TRANSFORM_SEM_HINT})
        action    = ProductionContainer("test action", [action_sen], data={SEMANTIC_HINT_V: ACTION_SEM_HINT})

        the_rule  = ProductionStructure("test rule",
                                        structure={
                                            QUERY_C     : query,
                                            TRANSFORM_C : transform,
                                            ACTION_C    : action
                                        },
                                        data={SEMANTIC_HINT_V: RULE_SEM_HINT})

        # insert a sentence into the struct
        sen = Sentence.build(["a", "test", "sentence"])
        trie_sem.func.insert(sen, trie_sem.struct)
        # run the rule
        result = semSys(the_rule, ctxs=ctx_set)

        # Check the struct doesn't change
        # check no action side effects occurred
        # check the returned context has a continuation

        # Run the continuation

        # check the action side effects occurreddef test_agenda(self):
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


if __name__ == '__main__':
    unittest.main()
