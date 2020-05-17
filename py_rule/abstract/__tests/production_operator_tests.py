#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.abstract import production_operator as PO


class ProductionOperatorTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
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

    @unittest.skip("TODO")
    def test_component_init(self):
        pass

    @unittest.skip("TODO")
    def test_component_call(self):
        pass

    @unittest.skip("TODO")
    def test_component_op(self):
        pass

    @unittest.skip("TODO")
    def test_component_var_set(self):
        pass

    @unittest.skip("TODO")
    def test_apply_parameters(self):
        pass

    @unittest.skip("TODO")
    def test_set_rebind(self):
        pass

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
