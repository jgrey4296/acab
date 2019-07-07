import unittest
import logging
from test_context import py_rule
from py_rule.typing.type_checker import TypeChecker

class (unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_init(self):

    def test_typing_context(self):

    def test_add_definition(self):

    def test_add_assertion(self):

    def test_add_rule(self):

    def test_basic_inference(self):

    def test_basic_component_inference(self):

    def test_type_conflict(self):

    def test_type_undefined(self):

    def test_type_redefinition(self):

    def test_variable_conflict(self):

    def test_structure_mismatch(self):

    def test_structure_type_conflict(self):

    def test_typing_nested_vars(self):

    def test_typing_nested_types(self):

    def test_typing_nested_types_fail(self):

    def test_typing_polytype(self):

    def test_typing_polytype_nested(self):

    def test_typing_polytype_nested_fail(self):





if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log."
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
