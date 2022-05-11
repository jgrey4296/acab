#!/opt/anaconda3/envs/acab/bin/python
import sys
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))
import logging as logmod
import unittest
import unittest.mock as mock
from enum import Enum
from os.path import split, splitext

import acab

logging = logmod.getLogger(__name__)
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

import acab.modules.semantics.statements as ASem
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.node import AcabNode
from acab.core.semantics import basic
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                                OperatorResultWrap)
from acab.core.value import default_structure as DS
from acab.core.value.instruction import (ActionOperator, Instruction,
                                         ProductionComponent,
                                         ProductionContainer,
                                         ProductionOperator,
                                         ProductionStructure)
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.error.base import AcabBasicException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.semantic import SemanticSystem_i, StatementSemantics_i
from acab.interfaces.value import ValueFactory
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import (ConstraintCollection,
                                              ContextInstance, ContextSet)
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag
from acab.modules.operators.transform.transform_operators import RegexOp
from acab.modules.semantics import default
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.semantics import FlattenBreadthTrieSemantics

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, _type=Enum)()

NEGATION_V      = DS.NEGATION
BIND_V          = DS.BIND
CONSTRAINT_V    = DS.CONSTRAINT
SEMANTIC_HINT_V = DS.SEMANTIC_HINT
QUERY_V         = DS.QUERY

QUERY_C      = DS.QUERY_COMPONENT
TRANSFORM_C  = DS.TRANSFORM_COMPONENT
ACTION_C     = DS.ACTION_COMPONENT


QUERY_SIGNAL     = Sentence() << config.attr.Semantic.Signals.QUERY
ACTION_SIGNAL    = Sentence() << config.attr.Semantic.Signals.ACTION
TRANSFORM_SIGNAL = Sentence() << config.attr.Semantic.Signals.TRANSFORM
RULE_SIGNAL      = Sentence() << config.attr.Semantic.Signals.RULE
AGENDA_SIGNAL    = Sentence() << config.attr.Semantic.Signals.AGENDA
LAYER_SIGNAL     = Sentence() << config.attr.Semantic.Signals.LAYER
PIPELINE_SIGNAL  = Sentence() << config.attr.Semantic.Signals.PIPELINE

# TODO test verify

class StubAbsSemantic(basic.StatementSemantics, StatementSemantics_i):
    def __call__(self, ins, semSys, ctxs=None, data=None):
        raise AcabBasicException("TestAbsSem called")


class StatementSemanticTests(unittest.TestCase):
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

    def test_transform(self):
        """ Check transforms semantics work """
        sem                                 = ASem.TransformAbstraction()
        # Construct context set for operators
        op_loc_path                         = Sentence(["Regex", "Operation"])
        operator_instance                   = RegexOp()
        op_ctx                              = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set                             = ContextSet(op_ctx)
        # Add a ContextInst
        init_ctx                            = ctx_set.pop()
        updated_ctx                         = init_ctx.bind_dict({"x" : AcabValue("test")})
        ctx_set.push(updated_ctx)
        # Build Transform
        rebind_target                       = AcabValue("y", data={BIND_V: True})
        clause                              = ProductionComponent(op_loc_path,
                                                                  params=["x", "es", "ES"],
                                                                  rebind =rebind_target)

        transform                           = ProductionContainer([clause])
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
        op_loc_path       = Sentence(["action"])
        operator_instance = TestAction()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet(op_ctx)
        # Build Action
        clause = ProductionComponent(op_loc_path)
        action = ProductionContainer([clause])
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
        op_loc_path       = Sentence(["action"])
        operator_instance = TestAction()
        op_ctx            = ContextInstance(data={str(op_loc_path): operator_instance})
        ctx_set           = ContextSet(op_ctx)
        # Build Action
        clause = ProductionComponent(op_loc_path, params=["awef"])
        action = ProductionContainer([clause])
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


        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return str(val.value)

        # Specs
        default_spec  = BasicSemanticSystem.Spec(DEFAULT_HANDLER_SIGNAL).spec_from(StatementSemantics_i)
        all_specs     = default.DEFAULT_SPECS() + [default_spec]
        # Semantics
        transform_sem = ASem.TransformAbstraction().as_handler(signal=TRANSFORM_SIGNAL)
        action_sem    = ASem.ActionAbstraction().as_handler(signal=ACTION_SIGNAL)
        stub_sem      = StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)
        con_sem       = ASem.ContainerAbstraction().as_handler(signal="CONTAINER")

        semSys        = BasicSemanticSystem(init_specs=all_specs,
                                            init_handlers=[transform_sem,
                                                           action_sem,
                                                           stub_sem,
                                                           con_sem]
                                            )

        # Operator Context
        op_ctx             = ContextInstance(data={"transform" : TestTransform(),
                                                   "action"    : TestAction()})

        # Add data to eval context
        ctx_set     = ContextSet(op_ctx)
        init_ctx    = ctx_set.pop()
        updated_ctx = init_ctx.bind_dict({
            "x" : AcabValue("test")
        })
        ctx_set.push(updated_ctx)

        # Build Instruction to run
        rebind_target          = AcabValue("y", data={BIND_V: True})
        transform_clause       = ProductionComponent(Sentence(["transform"]), params=["x"], rebind=rebind_target)
        action_clause          = ProductionComponent(Sentence(["action"]), params=["y"])
        container_instruction  = ProductionContainer([ProductionContainer([transform_clause], data={SEMANTIC_HINT_V: TRANSFORM_SIGNAL}),
                                                      ProductionContainer([action_clause], data={SEMANTIC_HINT_V: ACTION_SIGNAL})])

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


        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None

        # Build Semantics
        node_sem     = BasicNodeSemantics().as_handler(signal="atom")
        trie_sem     = FlattenBreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_handler = trie_sem.as_handler(signal="trie", struct=BasicNodeStruct.build_default())


        query_sem   = ASem.QueryAbstraction().as_handler(signal=QUERY_SIGNAL)
        action_sem  = ASem.ActionAbstraction().as_handler(signal=ACTION_SIGNAL)
        rule_sem    = ASem.AtomicRuleAbstraction().as_handler(signal=RULE_SIGNAL)
        trans_sem   = ASem.TransformAbstraction().as_handler(signal=TRANSFORM_SIGNAL)
        cont_sem    = ASem.ContainerAbstraction().as_handler(signal="CONTAINER")

        semSys      = BasicSemanticSystem(init_specs=default.DEFAULT_SPECS(),
                                          init_handlers=[cont_sem,
                                                         query_sem,
                                                         action_sem,
                                                         rule_sem,
                                                         trans_sem,
                                                         node_sem,
                                                         trie_handler,
                                                         trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)
                                                         ])

        # Setup operators in context
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={"action": TestAction(),
                                                   "regex" : trans_instance})
        ctx_set            = ContextSet(op_ctx)

        # Construct Rule
        query_sen                  = Sentence(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        query_sen.data[QUERY_V]    = True

        transform_sen = ProductionComponent(Sentence(["regex"]), params=["x", "sen", "SEN"],
                                            rebind=AcabValue("y", data={BIND_V: True}))
        action_sen    = ProductionComponent(Sentence([ "action" ]), params=['y'])

        query     = ProductionContainer([query_sen], name=QUERY_C, data={SEMANTIC_HINT_V: QUERY_SIGNAL})
        transform = ProductionContainer([transform_sen], name=TRANSFORM_C, data={SEMANTIC_HINT_V: TRANSFORM_SIGNAL})
        action    = ProductionContainer([action_sen], name=ACTION_C, data={SEMANTIC_HINT_V: ACTION_SIGNAL})

        the_rule  = ProductionStructure([query, transform, action],
                                        data={SEMANTIC_HINT_V: RULE_SIGNAL})

        # insert a sentence into the struct
        sen = Sentence(["a", "test", "sentence"])
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


        def SemHintKey(val, data=None):
            if SEMANTIC_HINT_V in val.data:
                return val.data[SEMANTIC_HINT_V]

            return None
        #
        # Build Semantics
        node_sem    = BasicNodeSemantics().as_handler(signal="atom")
        trie_sem    = FlattenBreadthTrieSemantics(init_handlers=[node_sem.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        trie_handler= trie_sem.as_handler(signal="trie",
                                          struct=BasicNodeStruct.build_default())

        query_sem   = ASem.QueryAbstraction().as_handler(signal=QUERY_SIGNAL)
        action_sem  = ASem.ActionAbstraction().as_handler(signal=ACTION_SIGNAL)
        trans_sem   = ASem.TransformAbstraction().as_handler(signal=TRANSFORM_SIGNAL)
        # THIS IS THE MAJOR CHANGE OF THIS TEST:
        rule_sem    = ASem.ProxyRuleAbstraction().as_handler(signal=RULE_SIGNAL)

        semSys      = BasicSemanticSystem(init_specs=default.DEFAULT_SPECS(),
                                          init_handlers=[query_sem,
                                                         action_sem,
                                                         trans_sem,
                                                         rule_sem,
                                                         node_sem,
                                                         trie_handler,
                                                         trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)
                                                         ])

        # Setup operators in context
        trans_instance     = RegexOp()
        op_ctx             = ContextInstance(data={"action" : TestAction(),
                                                   "regex"  : trans_instance})
        ctx_set            = ContextSet(op_ctx)

        # Construct Rule
        query_sen                  = Sentence(["a", "test", "x"])
        query_sen[-1].data[BIND_V] = True
        query_sen.data[QUERY_V]    = True

        transform_sen = ProductionComponent(Sentence("regex"), params=["x", "sen", "SEN"],
                                            rebind=AcabValue("y", data={BIND_V: True}))
        action_sen    = ProductionComponent(Sentence("action"), params=['y'])

        query     = ProductionContainer([query_sen], name=QUERY_C, data={SEMANTIC_HINT_V: QUERY_SIGNAL})
        transform = ProductionContainer([transform_sen], name=TRANSFORM_C, data={SEMANTIC_HINT_V: TRANSFORM_SIGNAL})
        action    = ProductionContainer([action_sen], name=ACTION_C, data={SEMANTIC_HINT_V: ACTION_SIGNAL})

        the_rule  = ProductionStructure([query, transform, action],
                                        data={SEMANTIC_HINT_V: RULE_SIGNAL})

        # insert a sentence into the struct
        sen = Sentence(["a", "test", "sentence"])
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
