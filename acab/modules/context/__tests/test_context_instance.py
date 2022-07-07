#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from __future__ import annotations

import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from weakref import ReferenceType
from os.path import split, splitext

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    import acab.core.defaults.value_keys as DS
    from acab.core.value.value import AcabValue
    from acab.error.context import AcabContextException
    from acab.modules.context.context_instance import ContextInstance
    from acab.modules.context.context_set import ContextSet
    from acab.core.data.node import AcabNode
    from acab.interfaces.value import ValueFactory as VF
    from acab.error.context import AcabContextException


class TestContextInstance(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h    = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logmod.root.handlers[0].setLevel(logmod.WARNING)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_instance_basic(self):
        """ Check a ContextInstance can be created """
        inst = ContextInstance()
        self.assertEqual(inst.data, {})
        self.assertEqual(inst.nodes, {})

    def test_instance_bindings(self):
        """ Check a context instance can be created with bindings """
        inst = ContextInstance(data={"a" : 2, "b" : 3})
        self.assertEqual(inst.data['a'], 2)
        self.assertEqual(inst.data['b'], 3)

    def test_instance_nodes(self):
        """ Check a context instance can be created with node bindings """
        inst = ContextInstance(nodes={"a": 2, "b": 3})
        self.assertEqual(inst.nodes["a"], 2)
        self.assertEqual(inst.nodes["b"], 3)

    def test_instance_copy(self):
        """ Check a context instance can be copied """
        inst = ContextInstance(data={"a": 2, "b": 3})
        inst2 = inst.copy()
        self.assertNotEqual(id(inst), id(inst2))
        self.assertNotEqual(inst.uuid, inst2.uuid)

    def test_instance_copy_tracks_parent_ctx(self):
        """ Check a context instance can be copied """
        inst = ContextInstance(data={"a": 2, "b": 3})
        inst2 = inst.copy()
        self.assertIsNone(inst._parent_ctx)
        self.assertIsInstance(inst2._parent_ctx, ReferenceType)
        self.assertIsInstance(inst2._parent_ctx(), ContextInstance)
        self.assertEqual(inst2._parent_ctx().uuid, inst.uuid)


    def test_instance_copy_independence(self):
        """ Check a context instance can be copied """
        inst = ContextInstance(data={"a": 2, "b": 3})
        inst2 = inst.copy()
        inst2.data["a"] = 5
        self.assertNotEqual(id(inst), id(inst2))
        self.assertNotEqual(inst.uuid, inst2.uuid)
        self.assertEqual(inst.data["a"], 2)
        self.assertEqual(inst2.data["a"], 5)


    def test_current_node(self):
        inst = ContextInstance()
        self.assertIsNone(inst.current_node)
        node = AcabNode(VF.value("test"))
        inst.set_current_node(node)
        self.assertIsNotNone(inst.current_node)

    def test_set_current_binding(self):
        inst = ContextInstance({"a": VF.value("blah")},
                               nodes={"a": AcabNode(VF.value("blah"))})
        self.assertIsNone(inst.current_node)
        inst.set_current_binding(VF.value("a"))
        self.assertIsNotNone(inst.current_node)

    def test_set_current_binding_fail(self):
        inst = ContextInstance({"a": VF.value("blah")})
        self.assertIsNone(inst.current_node)

        with self.assertRaises(AcabContextException) as cm:
            inst.set_current_binding(VF.value("a"))

        self.assertEqual(cm.exception.detail, "No Recognised binding")
        self.assertEqual(cm.exception.context[0], "a")

    def test_is_immutable(self):
        inst = ContextInstance()

        with self.assertRaises(TypeError) as cm:
            inst['a'] = "blah"


    def test_empty(self):
        inst = ContextInstance()
        self.assertFalse(inst)

    def test_not_empty(self):
        inst = ContextInstance({"a": VF.value("blah")})
        self.assertTrue(inst)


    def test_instance_contains(self):
        inst = ContextInstance({"a": VF.value("blah")})
        self.assertIn("a", inst)

    def test_instance_contains_fail(self):
        inst = ContextInstance({"a": VF.value("blah")})
        self.assertNotIn("b", inst)

    def test_instance_dict_progress(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        result = inst.progress({"b": VF.value("bloo")}, {})
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], ContextInstance)

        self.assertNotEqual(inst, result[0])
        self.assertNotIn("b", inst)
        self.assertIn("b", result[0])

    def test_instance_dict_progress_multi(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        result = inst.progress({"b": VF.value("bloo"),
                                "c" : VF.value("aweg")}, {})
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 1)
        self.assertIn("b", result[0])
        self.assertIn("c", result[0])

    def test_instance_val_progress(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        result = inst.progress(VF.value("b", data={DS.BIND: True}),
                               [AcabNode(VF.value("c"))])
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], ContextInstance)

        self.assertNotEqual(inst, result[0])
        self.assertNotIn("b", inst)
        self.assertIn("b", result[0])
        self.assertEqual(result[0]['b'], "c")

    def test_instance_val_progress_multi(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        result = inst.progress(VF.value("b", data={DS.BIND: True}),
                               [AcabNode(VF.value("c")),
                                AcabNode(VF.value("d"))])
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertIsInstance(result[0], ContextInstance)
        self.assertIsInstance(result[1], ContextInstance)

        self.assertNotEqual(inst, result[0])
        self.assertNotEqual(inst, result[1])
        self.assertNotIn("b", inst)
        self.assertIn("b", result[0])
        self.assertIn("b", result[1])
        self.assertEqual(result[0]['b'], "c")
        self.assertEqual(result[1]['b'], "d")

    def test_instance_progress_lineage(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        result = inst.progress(VF.value("b", data={DS.BIND: True}),
                               [AcabNode(VF.value("c")),
                                AcabNode(VF.value("d"))])
        last = result[0].progress({"c": VF.value("bloo")}, {})[0]

        self.assertEqual(len(result), 2)
        self.assertEqual(len(inst._lineage), 1)
        self.assertTrue(all(len(x._lineage) == 2 for x in result))
        self.assertTrue(all(inst.uuid in x._lineage for x in result))
        self.assertTrue(all(x.uuid not in inst._lineage for x in result))

        self.assertIn(inst.uuid, last._lineage)
        self.assertIn(result[0].uuid, last._lineage)
        self.assertTrue(all(last.uuid not in x._lineage for x in [inst] + result))

    def test_instance_depth(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        result = inst.progress(VF.value("b", data={DS.BIND: True}),
                               [AcabNode(VF.value("c")),
                                AcabNode(VF.value("d"))])
        last = result[0].progress({"c": VF.value("bloo")}, {})[0]

        self.assertEqual(inst._depth, 1)
        self.assertTrue(all(x._depth == 2 for x in result))
        self.assertEqual(last._depth, 3)
