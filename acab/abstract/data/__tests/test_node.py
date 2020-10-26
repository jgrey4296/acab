#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue
from acab.abstract.core.type_system import build_simple_type_system

BIND_S = AcabConfig.Get()("Parsing.Structure", "BIND_S")
AV = AcabValue

class AcabNodeTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # setup class
        type_sys = build_simple_type_system()
        AcabValue._set_type_system(type_sys)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_basic_creation(self):
        a_node = AcabNode(AV("test"))
        self.assertIsNotNone(a_node)

    def test_length(self):
        a_node = AcabNode(AV("test"))
        self.assertEqual(len(a_node), 0)
        a_node._children['a child'] = True
        self.assertEqual(len(a_node), 1)
        a_node._children['another child'] = True
        self.assertEqual(len(a_node), 2)

    def test_bool(self):
        a_node = AcabNode(AV("test"))
        self.assertEqual(bool(a_node), False)
        a_node._children['a child'] = True
        self.assertEqual(bool(a_node), True)
        a_node._children['another child'] = True
        self.assertEqual(bool(a_node), True)

    def test_contains_true(self):
        a_node = AcabNode(AV("value"))
        a_node._children['child'] = True
        self.assertTrue(a_node.has_child('child'))

    def test_contains_false(self):
        a_node = AcabNode(AV("value"))
        self.assertFalse(a_node.has_child('blah'))

    def test_add_child(self):
        a_node = AcabNode(AV('value'))
        b_node = AcabNode(AV('value2'))
        self.assertFalse(bool(a_node))
        self.assertEqual(len(a_node), 0)
        a_node.add_child(b_node)
        self.assertTrue(bool(a_node))
        self.assertEqual(len(a_node), 1)
        self.assertTrue(a_node.has_child('value2'))

    def test_has_child(self):
        a_node = AcabNode(AV('value'))
        b_node = AcabNode(AV('value2'))
        self.assertFalse(bool(a_node))
        self.assertEqual(len(a_node), 0)
        a_node.add_child(b_node)
        self.assertTrue(bool(a_node))
        self.assertEqual(len(a_node), 1)
        self.assertTrue(a_node.has_child(b_node))

    def test_get_child(self):
        a_node = AcabNode(AV('value'))
        b_node = AcabNode(AV('value2'))
        self.assertFalse(bool(a_node))
        self.assertEqual(len(a_node), 0)
        a_node.add_child(b_node)
        self.assertTrue(bool(a_node))
        self.assertEqual(len(a_node), 1)
        self.assertTrue(a_node.has_child(b_node))
        gotten_node = a_node.get_child(b_node)
        gotten_node_b = a_node.get_child('value2')
        self.assertEqual(gotten_node, b_node)
        self.assertEqual(gotten_node_b, b_node)

    def test_remove_child(self):
        a_node = AcabNode(AV('value'))
        b_node = AcabNode(AV('value2'))
        self.assertFalse(bool(a_node))
        self.assertEqual(len(a_node), 0)
        a_node.add_child(b_node)
        self.assertTrue(bool(a_node))
        self.assertEqual(len(a_node), 1)
        self.assertTrue(a_node.has_child(b_node))
        a_node.remove_child(b_node)
        self.assertEqual(len(a_node), 0)
        self.assertFalse(bool(a_node))

    def test_clear_children(self):
        a_node = AcabNode(AV('value'))
        b_node = AcabNode(AV('value2'))
        c_node = AcabNode(AV('value3'))
        self.assertFalse(bool(a_node))
        self.assertEqual(len(a_node), 0)
        a_node.add_child(b_node)
        a_node.add_child(c_node)
        self.assertEqual(len(a_node),2)
        a_node.clear_children()
        self.assertEqual(len(a_node),0)
        self.assertFalse(bool(a_node))


    def test_var_set(self):
        node = AcabNode(AV("test", data={BIND_S: True}))
        var_set = node.var_set
        var_set_str = [x.name for x in var_set['out']]
        self.assertTrue("test" in var_set_str)

    def test_nested_var_set(self):
        node = AcabNode(AV("inner", data={BIND_S: True}))
        var_set = node.var_set
        var_set_str = [x.name for x in var_set['in']]
        self.assertTrue('inner' in var_set_str)

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
