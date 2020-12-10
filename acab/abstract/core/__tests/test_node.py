#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)
from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue

BIND_S = AcabConfig.Get().value("Value.Structure", "BIND")
AV = AcabValue

class AcabNodeTests(unittest.TestCase):

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
        a_node.children['a child'] = True
        self.assertEqual(len(a_node), 1)
        a_node.children['another child'] = True
        self.assertEqual(len(a_node), 2)

    def test_bool(self):
        a_node = AcabNode(AV("test"))
        self.assertEqual(bool(a_node), False)
        a_node.children['a child'] = True
        self.assertEqual(bool(a_node), True)
        a_node.children['another child'] = True
        self.assertEqual(bool(a_node), True)

    def test_contains_true(self):
        a_node = AcabNode(AV("value"))
        a_node.children['child'] = True
        self.assertTrue(a_node.has_child('child'))

    def test_contains_false(self):
        a_node = AcabNode(AV("value"))
        self.assertFalse(a_node.has_child('blah'))

    def test_add_child(self):
        a_val = AV(name='value')
        b_val = AV(name='value2')
        a_node = AcabNode(value=a_val, data={})
        b_node = AcabNode(value=b_val)
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




    # root, str, iter,
    # set_parent, parentage,
