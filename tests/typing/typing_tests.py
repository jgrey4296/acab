import unittest
import logging
from test_context import py_rule
from py_rule.typing.type_checker import TypeChecker

class TypingTests(unittest.TestCase):

    def setUp(self):
	    return 1

    def tearDown(self):
	    return 1

    #----------
    def test_init(self):
        return

    def test_typing_context(self):
	    return

    def test_add_definition(self):
	    return

    def test_add_assertion(self):
	    return

    def test_add_rule(self):
	    return

    def test_basic_inference(self):
	    return

    def test_basic_component_inference(self):
	    return

    def test_type_conflict(self):
	    return

    def test_type_undefined(self):
	    return

    def test_type_redefinition(self):
	    return

    def test_variable_conflict(self):
	    return

    def test_structure_mismatch(self):
	    return

    def test_structure_type_conflict(self):
	    return

    def test_typing_nested_vars(self):
	    return

    def test_typing_nested_types(self):
	    return

    def test_typing_nested_types_fail(self):
	    return

    def test_typing_polytype(self):
	    return

    def test_typing_polytype_nested(self):
	    return

    def test_typing_polytype_nested_fail(self):
	    return




if __name__ == "__main__":
	#use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.typing_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
