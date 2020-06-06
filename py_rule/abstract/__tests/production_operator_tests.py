#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.abstract import production_operator as PO
from py_rule.abstract.value import PyRuleValue, PyRuleStatement


class ProductionOperatorTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    @unittest.skip("TODO")
    def test_verify(self):
        # create an action with numerous action components,
        pass



    @unittest.skip("TODO")
    def test_subclass_tree(self):
        pass

    @unittest.skip("TODO")
    def test_op_str(self):
        pass


    def test_component_init(self):
        val = PO.ProductionComponent("testop", [])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertIsInstance(val, PyRuleValue)

    @unittest.skip("TODO")
    def test_component_call(self):
        pass

    def test_component_op(self):
        val = PO.ProductionComponent("testop", [])
        self.assertEqual(val.op, "testop")

    @unittest.skip("TODO")
    def test_component_var_set(self):
        pass


    def test_apply_parameters(self):
        val = PO.ProductionComponent("testop", [])
        self.assertEqual(len(val._params), 0)
        val.apply_params(["a","test"])
        self.assertEqual(len(val._params), 2)

    @unittest.skip("TODO")
    def test_get_params(self):
        pass

    @unittest.skip("TODO")
    def test_to_sentence(self):
        pass


    @unittest.skip("TODO")
    def test_container_init(self):
        pass

    @unittest.skip("TODO")
    def test_container_call(self):
        pass

    @unittest.skip("TODO")
    def test_container_var_set(self):
        pass

    @unittest.skip("TODO")
    def test_get_vars(self):
        pass

    @unittest.skip("TODO")
    def test_container_to_sentences(self):
        pass


    @unittest.skip("TODO")
    def test_refine_op_func(self):
        return

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
