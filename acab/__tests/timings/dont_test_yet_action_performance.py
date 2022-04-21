#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)


import acab

acab.setup()

from acab.core.value.instruction import ProductionContainer, ProductionOperator
from acab.core.value.value import AcabValue
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.engine.engine import Engine
from acab.core.parsing import consts as PConst
from acab.core.printing import default_handlers as DH
from acab.core.semantics.print_semantics import AcabPrintSemantics
from acab.modules.operators.action import action_operators as act_ops
from acab.modules.parsing.exlo import ActionParser as AP

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop'})

from acab.core.semantics.production_semantics import ProductionSemantics

# TODO production semantics
ProdSem = ProductionSemantics()

def S(*words):
    return Sentence.build(words)

class ActionBlah(ProductionOperator):
    def __call__(self, engine, params):
        logging.info("Blah")


class ActionTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)


    def setUp(self):
        self.e = Engine(modules=["acab.modules.operators.standard_operators"])
        self.e.alias_module(S("acab","modules","operators", "standard", "operators"), S("A"))
        self.e.register_ops([S("Blah").attach_statement(ActionBlah())])
        self.e.build()

    def tearDown(self):
        return 1

    #----------
    def test_add_to_wm(self):
        self.assertFalse(self.e.query("a.b.c?"))
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))

    def test_run_assert_action(self):
        actions = AP.parse_string("λA.ActionAdd a.b.c")
        self.assertFalse(self.e.query("a.b.c?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.c?"))

    def test_run_retract_action(self):
        actions = AP.parse_string("λA.ActionAdd ~a.b.c")
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))
        results = ProdSem(actions, {}, self.e)
        self.assertFalse(self.e.query("a.b.c?"))
        self.assertTrue(self.e.query("~a.b.c?"))

    def test_run_assert_multi_action(self):
        actions = AP.parse_string("λA.ActionAdd a.b.c,λA.ActionAdd a.b.d")
        self.assertFalse(self.e.query("a.b.c?, a.b.d?"))
        self.assertTrue(self.e.query("~a.b.c?, ~a.b.d?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.c?, a.b.d?"))

    def test_run_mixed_multi_action(self):
        actions = AP.parse_string("λA.ActionAdd a.b.c, λA.ActionAdd ~a.b.d")
        self.e.add("a.b.d")
        self.assertTrue(self.e.query("~a.b.c?, a.b.d?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.c?, ~a.b.d?"))

    def test_run_bound_assert_action(self):
        data = {"x": "blah"}
        actions = AP.parse_string("λA.ActionAdd a.b.$x")
        self.assertTrue(self.e.query("~a.b.blah?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.blah?"))


    def test_run_bound_retract_action(self):
        data = {"blah" : "bloo"}
        actions = AP.parse_string("λA.ActionAdd ~a.$blah.c")
        self.e.add("a.bloo.c")
        self.assertTrue(self.e.query("a.bloo.c?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("~a.bloo.c?, a.bloo?"))

    def test_run_mixed_bound_actions(self):
        data = {"blah": "bloo"}
        actions = AP.parse_string("λA.ActionAdd a.$blah, λA.ActionAdd ~b.$blah")
        self.e.add("b.bloo")
        self.assertTrue(self.e.query("b.bloo?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.bloo?, ~b.bloo?"))

    def test_custom_action_parse(self):
        result = AP.parse_string(r"λBase.blah a b c")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, ProductionContainer)

        self.assertEqual(Printer.print(result.clauses[0].op), "Base.blah")
        self.assertEqual(result.clauses[0].params, ['a', 'b', 'c'])
