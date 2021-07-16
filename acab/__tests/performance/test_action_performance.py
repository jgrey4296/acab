#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)


import acab
acab.setup()

from acab.abstract.parsing import consts as PConst

from acab.abstract.core.values import  AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionContainer
from acab.abstract.parsing.trie_bootstrapper import TrieBootstrapper

from acab.abstract.engine.engine import Engine
from acab.modules.parsing.exlo import ActionParser as AP
from acab.modules.operators.action import action_operators as act_ops

from acab.abstract.semantics.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop'})

from acab.abstract.semantics.production_semantics import ProductionSemantics

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
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)


    def setUp(self):
        self.e = Engine(modules=["acab.modules.operators.standard_operators"])
        self.e.alias_module(S("acab","modules","operators", "standard", "operators"), S("A"))
        self.e.register_ops([S("Blah").attach_statement(ActionBlah())])
        self.e.build_DSL()

    def tearDown(self):
        return 1

    #----------
    def test_add_to_wm(self):
        self.assertFalse(self.e.query("a.b.c?"))
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))

    def test_run_assert_action(self):
        actions = AP.parseString("λA.ActionAdd a.b.c")
        self.assertFalse(self.e.query("a.b.c?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.c?"))

    def test_run_retract_action(self):
        actions = AP.parseString("λA.ActionAdd ~a.b.c")
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))
        results = ProdSem(actions, {}, self.e)
        self.assertFalse(self.e.query("a.b.c?"))
        self.assertTrue(self.e.query("~a.b.c?"))

    def test_run_assert_multi_action(self):
        actions = AP.parseString("λA.ActionAdd a.b.c,λA.ActionAdd a.b.d")
        self.assertFalse(self.e.query("a.b.c?, a.b.d?"))
        self.assertTrue(self.e.query("~a.b.c?, ~a.b.d?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.c?, a.b.d?"))

    def test_run_mixed_multi_action(self):
        actions = AP.parseString("λA.ActionAdd a.b.c, λA.ActionAdd ~a.b.d")
        self.e.add("a.b.d")
        self.assertTrue(self.e.query("~a.b.c?, a.b.d?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.c?, ~a.b.d?"))

    def test_run_bound_assert_action(self):
        data = {"x": "blah"}
        actions = AP.parseString("λA.ActionAdd a.b.$x")
        self.assertTrue(self.e.query("~a.b.blah?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.b.blah?"))


    def test_run_bound_retract_action(self):
        data = {"blah" : "bloo"}
        actions = AP.parseString("λA.ActionAdd ~a.$blah.c")
        self.e.add("a.bloo.c")
        self.assertTrue(self.e.query("a.bloo.c?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("~a.bloo.c?, a.bloo?"))

    def test_run_mixed_bound_actions(self):
        data = {"blah": "bloo"}
        actions = AP.parseString("λA.ActionAdd a.$blah, λA.ActionAdd ~b.$blah")
        self.e.add("b.bloo")
        self.assertTrue(self.e.query("b.bloo?"))
        results = ProdSem(actions, {}, self.e)
        self.assertTrue(self.e.query("a.bloo?, ~b.bloo?"))

    def test_custom_action_parse(self):
        result = AP.parseString(r"λBase.blah a b c")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, ProductionContainer)

        self.assertEqual(Printer.print(result.clauses[0].op), "Base.blah")
        self.assertEqual(result.clauses[0].params, ['a', 'b', 'c'])
