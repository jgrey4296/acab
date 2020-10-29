#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.sentence import Sentence
from acab.abstract.rule import action
from acab.abstract.rule.production_operator import ProductionOperator
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.core.type_system import build_simple_type_system

from acab.engines.trie_engine import TrieEngine
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.modules.operators.action import action_operators as act_ops

def S(*words):
    return Sentence.build(words)

class ActionBlah(action.ActionOp):
    def __init__(self):
        super().__init__()

    def __call__(self, engine, params):
        logging.info("Blah")


class ActionTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # setup class
        type_sys = build_simple_type_system()


    def setUp(self):
        self.e = TrieEngine(modules=["acab.modules.operators.standard_operators"])
        self.e.alias_module(S("acab","modules","operators", "standard", "operators"), S("A"))
        self.e.register_ops([S("Blah").attach_statement(ActionBlah())])
        self.e.build_DSL()

    def tearDown(self):
        return 1

    #----------
    def test_run_assert_action(self):
        actions = AP.parseString("λA.ActionAdd a.b.c")
        self.assertFalse(self.e.query("a.b.c?"))
        actions({}, self.e)
        self.assertTrue(self.e.query("a.b.c?"))

    def test_run_retract_action(self):
        actions = AP.parseString("λA.ActionAdd ~a.b.c")
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))
        actions({}, self.e)
        self.assertFalse(self.e.query("a.b.c?"))
        self.assertTrue(self.e.query("~a.b.c?"))

    def test_run_assert_multi_action(self):
        actions = AP.parseString("λA.ActionAdd a.b.c,λA.ActionAdd a.b.d")
        self.assertFalse(self.e.query("a.b.c?, a.b.d?"))
        self.assertTrue(self.e.query("~a.b.c?, ~a.b.d?"))
        actions({}, self.e)
        self.assertTrue(self.e.query("a.b.c?, a.b.d?"))

    def test_run_mixed_multi_action(self):
        actions = AP.parseString("λA.ActionAdd a.b.c, λA.ActionAdd ~a.b.d")
        self.e.add("a.b.d")
        self.assertTrue(self.e.query("~a.b.c?, a.b.d?"))
        actions({}, self.e)
        self.assertTrue(self.e.query("a.b.c?, ~a.b.d?"))

    def test_run_bound_assert_action(self):
        data = {"x": "blah"}
        actions = AP.parseString("λA.ActionAdd a.b.$x")
        self.assertTrue(self.e.query("~a.b.blah?"))
        actions(data, self.e)
        self.assertTrue(self.e.query("a.b.blah?"))


    def test_run_bound_retract_action(self):
        data = {"blah" : "bloo"}
        actions = AP.parseString("λA.ActionAdd ~a.$blah.c")
        self.e.add("a.bloo.c")
        self.assertTrue(self.e.query("a.bloo.c?"))
        actions(data, self.e)
        self.assertTrue(self.e.query("~a.bloo.c?, a.bloo?"))

    def test_run_mixed_bound_actions(self):
        data = {"blah": "bloo"}
        actions = AP.parseString("λA.ActionAdd a.$blah, λA.ActionAdd ~b.$blah")
        self.e.add("b.bloo")
        self.assertTrue(self.e.query("b.bloo?"))
        actions(data, self.e)
        self.assertTrue(self.e.query("a.bloo?, ~b.bloo?"))

    def test_custom_action_parse(self):
        result = AP.parseString(r"λBase.blah a b c")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, action.Action)

        self.assertEqual(result.clauses[0].op.pprint(), "Base.blah")
        self.assertEqual([x.pprint() for x in result.clauses[0]._params], ['a', 'b', 'c'])




if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
