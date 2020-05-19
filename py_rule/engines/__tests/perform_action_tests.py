#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.engines.trie_engine import TrieEngine
from py_rule.working_memory.trie_wm.parsing import ActionParser as AP
from py_rule.modules.operators.action import action_operators as act_ops
from py_rule.abstract import action
from py_rule.abstract.bootstrap_parser import BootstrapParser
from py_rule.abstract.production_operator import ProductionOperator
from py_rule.modules.operators.standard_operators import StandardOperators

class ActionBlah(action.ActionOp):
    def __init__(self):
        super().__init__()

    def __call__(self, engine, params):
        logging.info("Blah")


class ActionTests(unittest.TestCase):

    def setUp(self):
        self.e = TrieEngine(modules=[StandardOperators()])
        self.e._working_memory._bootstrap_parser.add("operator.action.blah", ActionBlah)
        self.e.reload_all_modules()

    def tearDown(self):
        return 1

    #----------
    def test_run_assert_action(self):
        actions = AP.parseString("ActionAdd(a.b.c)")
        self.assertFalse(self.e.query("a.b.c?"))
        actions({}, self.e)
        self.assertTrue(self.e.query("a.b.c?"))

    def test_run_retract_action(self):
        actions = AP.parseString("ActionAdd(~a.b.c)")
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))
        actions({}, self.e)
        self.assertFalse(self.e.query("a.b.c?"))
        self.assertTrue(self.e.query("~a.b.c?"))

    def test_run_assert_multi_action(self):
        actions = AP.parseString("ActionAdd(a.b.c), ActionAdd(a.b.d)")
        self.assertFalse(self.e.query("a.b.c?, a.b.d?"))
        self.assertTrue(self.e.query("~a.b.c?, ~a.b.d?"))
        actions({}, self.e)
        self.assertTrue(self.e.query("a.b.c?, a.b.d?"))

    def test_run_mixed_multi_action(self):
        actions = AP.parseString("ActionAdd(a.b.c), ActionAdd(~a.b.d)")
        self.e.add("a.b.d")
        self.assertTrue(self.e.query("~a.b.c?, a.b.d?"))
        actions({}, self.e)
        self.assertTrue(self.e.query("a.b.c?, ~a.b.d?"))

    def test_run_bound_assert_action(self):
        data = {"x": "blah"}
        actions = AP.parseString("ActionAdd(a.b.$x)")
        self.assertTrue(self.e.query("~a.b.blah?"))
        actions(data, self.e)
        self.assertTrue(self.e.query("a.b.blah?"))

    def test_run_bound_retract_action(self):
        data = {"blah" : "bloo"}
        actions = AP.parseString("ActionAdd(~a.$blah.c)")
        self.e.add("a.bloo.c")
        self.assertTrue(self.e.query("a.bloo.c?"))
        actions(data, self.e)
        self.assertTrue(self.e.query("~a.bloo.c?, a.bloo?"))

    def test_run_mixed_bound_actions(self):
        data = {"blah": "bloo"}
        actions = AP.parseString("ActionAdd(a.$blah), ActionAdd(~b.$blah)")
        self.e.add("b.bloo")
        self.assertTrue(self.e.query("b.bloo?"))
        actions(data, self.e)
        self.assertTrue(self.e.query("a.bloo?, ~b.bloo?"))

    def test_custom_action_parse(self):
        result = AP.parseString("ActionBlah(a, b, c)")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, action.Action)
        self.assertEqual(result.clauses[0].op, "ActionBlah")
        self.assertEqual([x.value for x in result.clauses[0]._params], ['a', 'b', 'c'])




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
