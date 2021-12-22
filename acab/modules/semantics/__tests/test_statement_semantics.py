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

import acab.modules.semantics.statements as ASem
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.node import AcabNode
from acab.core.data.instruction import (ActionOperator,
                                                        ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.core.data.value import AcabValue, Sentence
from acab.core.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorResultWrap)
from acab.interfaces.handler_system import Handler
from acab.interfaces.semantic import (StatementSemantics_i,
                                      SemanticSystem_i)
from acab.error.acab_exception import AcabBasicException
from acab.error.semantic_exception import AcabSemanticException
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import (ConstraintCollection,
                                              ContextInstance, ContextSet)
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag
from acab.modules.operators.transform.transform_operators import RegexOp
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.structures.trie.trie_semantics import BreadthTrieSemantics
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.semantics import default

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

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

class StatementSemanticTests(unittest.TestCase):
    def test_transform(self):
        """ Check transforms semantics work """
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
        """ check container semantics applies all clauses """
        side_effect_obj = {"a" : 1}

        class TestTransform(ProductionOperator):
            @OperatorArgUnWrap
            @OperatorResultWrap
            def __call__(self, *params, data=None):
                return params[0] + "_blah"

        class TestAction(ActionOperator):
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(StatementSemantics_i):
            def __call__(self, ins, semSys, ctxs=None, data=None):
                breakpoint()
                raise AcabBasicException("TestAbsSem called")

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return str(val.value)

        # Specs
        default_spec  = BasicSemanticSystem.Spec(DEFAULT_HANDLER_SIGNAL).spec_from(StatementSemantics_i)
        all_specs     = default.DEFAULT_SPECS() + [default_spec]
        # Semantics
        transform_sem = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
        action_sem    = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
        stub_sem      = StubAbsSemantic().as_handler(DEFAULT_HANDLER_SIGNAL)
        con_sem        = ASem.ContainerAbstraction().as_handler("CONTAINER")

        semSys        = BasicSemanticSystem(init_specs=all_specs,
                                            init_handlers=[transform_sem,
                                                           action_sem,
                                                           stub_sem,
                                                           con_sem])

        # Operator Context
        op_ctx             = ContextInstance(data={"transform" : TestTransform(),
                                                   "action"    : TestAction()})

        # Add data to eval context
        ctx_set     = ContextSet.build(op_ctx)
        init_ctx    = ctx_set.pop()
        updated_ctx = init_ctx.bind_dict({
            "x" : AcabValue.safe_make("test")
        })
        ctx_set.push(updated_ctx)

        # Build Instruction to run
        rebind_target          = AcabValue.safe_make("y", data={BIND_V: True})
        transform_clause       = ProductionComponent("transform test",
                                                     Sentence.build(["transform"]),
                                                     ["x"],
                                                     rebind=rebind_target)
        action_clause          = ProductionComponent("Test Action Clause",
                                                     Sentence.build(["action"]),
                                                     ["y"])
        container_instruction  = ProductionContainer("mixed_container",
                                                     [ProductionContainer("transform", [transform_clause],
                                                                          data={SEMANTIC_HINT_V: TRANSFORM_SEM_HINT}),
                                                      ProductionContainer("action", [action_clause],
                                                                          data={SEMANTIC_HINT_V: ACTION_SEM_HINT})])

        # run each element of container with semantics
        semSys(container_instruction, ctxs=ctx_set)

        # check result
        # orig val "test" + transform "_blah" into side effect obj "a"
        self.assertEqual(side_effect_obj["a"], "test_blah")


    def test_atomic_rule(self):
        """ Check a rule can be applied in an atomic step """
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            @OperatorArgUnWrap
            @OperatorResultWrap
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(StatementSemantics_i):
            def __call__(self, ins, semSys, ctxs=None, data=None):
                raise AcabException("TestAbsSem called", rest=[str(ins), data])

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None

        # Build Semantics
        node_sem     = BasicNodeSemantics().as_handler("atom")
        trie_sem     = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_handler = trie_sem.as_handler("trie", struct=BasicNodeStruct.build_default())


        query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
        action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
        rule_sem    = ASem.AtomicRuleAbstraction().as_handler(RULE_SEM_HINT)
        trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
        cont_sem    = ASem.ContainerAbstraction().as_handler("CONTAINER")

        semSys      = BasicSemanticSystem(init_specs=default.DEFAULT_SPECS(),
                                          init_handlers=[cont_sem,
                                                         query_sem,
                                                         action_sem,
                                                         rule_sem,
                                                         trans_sem,
                                                         node_sem,
                                                         trie_handler,
                                                         trie_handler.as_handler(DEFAULT_HANDLER_SIGNAL)
                                                         ])

        # Setup operators in context
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={"action": TestAction(),
                                                   "regex" : trans_instance})
        ctx_set            = ContextSet.build(op_ctx)

        # Construct Rule
        query_sen = Sentence.build(["a", "test", "x"])
        query_sen[-1].data[BIND_V]  = True
        query_sen[-1].data[QUERY_V] = True

        transform_sen = ProductionComponent("transform_test",
                                            Sentence.build(["regex"]),
                                            ["x", "sen", "SEN"],
                                            rebind=AcabValue.safe_make("y", data={BIND_V: True}))
        action_sen    = ProductionComponent("Test Action Clause",
                                            Sentence.build([ "action" ]),
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
        trie_handler.func.insert(sen, trie_handler.struct)
        # run the rule
        result = semSys(the_rule, ctxs=ctx_set)

        # Check the result for bindings
        self.assertEqual(side_effect_obj["a"], "SENtence")

    def test_proxy_rule(self):
        """ Check a rule can be applied in a two stage, non-atomic fashion """
        side_effect_obj = {"a" : 1}

        class TestAction(ActionOperator):
            @OperatorArgUnWrap
            def __call__(self, *params, data=None, semSystem=None):
                side_effect_obj['a'] = params[0]

        class StubAbsSemantic(StatementSemantics_i):
            def __call__(self, ins, semSys, ctxs=None, data=None):
                raise AcabException("TestAbsSem called", rest=[str(ins), data])

        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None
        #
        # Build Semantics
        node_sem    = BasicNodeSemantics().as_handler("atom")
        trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])
        trie_handler= trie_sem.as_handler("trie",
                                          struct=BasicNodeStruct.build_default())

        query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
        action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
        trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
        # THIS IS THE MAJOR CHANGE OF THIS TEST:
        rule_sem    = ASem.ProxyRuleAbstraction().as_handler(RULE_SEM_HINT)

        semSys      = BasicSemanticSystem(init_specs=default.DEFAULT_SPECS(),
                                          init_handlers=[query_sem,
                                                         action_sem,
                                                         trans_sem,
                                                         rule_sem,
                                                         node_sem,
                                                         trie_handler,
                                                         trie_handler.as_handler(DEFAULT_HANDLER_SIGNAL)
                                                         ])

        # Setup operators in context
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={"action" : TestAction(),
                                                   "regex"  : trans_instance})
        ctx_set            = ContextSet.build(op_ctx)

        # Construct Rule
        query_sen = Sentence.build(["a", "test", "x"])
        query_sen[-1].data[BIND_V]  = True
        query_sen[-1].data[QUERY_V] = True

        transform_sen = ProductionComponent("transform_test",
                                            Sentence.build("regex"),
                                            ["x", "sen", "SEN"],
                                            rebind=AcabValue.safe_make("y", data={BIND_V: True}))
        action_sen    = ProductionComponent("Test Action Clause",
                                            Sentence.build("action"),
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
        trie_handler.func.insert(sen, trie_handler.struct)
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


    @unittest.skip("Not Implemented")
    def test_layer(self):
        # Build semantics

        # Build Struct

        # Build layer

        # Run Layer on struct

        # Check results
        pass

    @unittest.skip("Not Implemented")
    def test_pipeline(self):
        # build semantics

        # build struct

        # build pipeline

        # run pipeline

        # Check results
        pass




if __name__ == '__main__':
    unittest.main()
