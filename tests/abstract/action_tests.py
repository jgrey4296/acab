#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from test_context import py_rule


class (unittest.TestCase):

    @classmethod
    def setUpClas(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    @unittest.skip("Broken")
    def test_run_assert_action(self):
        actions = AP.parseString("+(a.b.c)")
        self.assertFalse(self.e.query("a.b.c?"))
        self.e._run_actions({},actions)
        self.assertTrue(self.e.query("a.b.c?"))

    @unittest.skip("Broken")
    def test_run_retract_action(self):
        actions = AP.parseString("-(a.b.c)")
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))
        self.e._run_actions({}, actions)
        self.assertFalse(self.e.query("a.b.c?"))
        self.assertTrue(self.e.query("~a.b.c?"))

    @unittest.skip("Broken")
    def test_run_assert_multi_action(self):
        actions = AP.parseString("+(a.b.c), +(a.b.d)")
        self.assertFalse(self.e.query("a.b.c?, a.b.d?"))
        self.assertTrue(self.e.query("~a.b.c?, ~a.b.d?"))
        self.e._run_actions({}, actions)
        self.assertTrue(self.e.query("a.b.c?, a.b.d?"))

    @unittest.skip("Broken")
    def test_run_mixed_multi_action(self):
        actions = AP.parseString("+(a.b.c), -(a.b.d)")
        self.e.add("a.b.d")
        self.assertTrue(self.e.query("~a.b.c?, a.b.d?"))
        self.e._run_actions({}, actions)
        self.assertTrue(self.e.query("a.b.c?, ~a.b.d?"))

    @unittest.skip("Broken")
    def test_run_bound_assert_action(self):
        data = {"x": "blah"}
        actions = AP.parseString("+(a.b.$x)")
        self.assertTrue(self.e.query("~a.b.blah?"))
        self.e._run_actions(data, actions)
        self.assertTrue(self.e.query("a.b.blah?"))

    @unittest.skip("Broken")
    def test_run_bound_retract_action(self):
        data = {"blah" : "bloo"}
        actions = AP.parseString("-(a.$blah.c)")
        self.e.add("a.bloo.c")
        self.assertTrue(self.e.query("a.bloo.c?"))
        self.e._run_actions(data, actions)
        self.assertTrue(self.e.query("~a.bloo.c?, a.bloo?"))

    @unittest.skip("Broken")
    def test_run_mixed_bound_actions(self):
        data = {"blah": "bloo"}
        actions = AP.parseString("+(a.$blah), -(b.$blah)")
        self.e.add("b.bloo")
        self.assertTrue(self.e.query("b.bloo?"))
        self.e._run_actions(data, actions)
        self.assertTrue(self.e.query("a.bloo?, ~b.bloo?"))



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
