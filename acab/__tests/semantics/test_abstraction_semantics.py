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


if __name__ == '__main__':
    unittest.main()
