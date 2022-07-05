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

    def test_instance_copy_independence(self):
        """ Check a context instance can be copied """
        inst = ContextInstance(data={"a": 2, "b": 3})
        inst2 = inst.copy()
        inst2.data["a"] = 5
        self.assertNotEqual(id(inst), id(inst2))
        self.assertNotEqual(inst.uuid, inst2.uuid)
        self.assertEqual(inst.data["a"], 2)
        self.assertEqual(inst2.data["a"], 5)

    def test_instance_bind(self):
        pass

    def test_instance_contains(self):
        pass

    def test_instance_getitem(self):
        pass


    def test_mutability_fail(self):
        pass

    def test_mutable_instance(self):
        pass

    def test_progress(self):
        pass

    def instance_context_handler(self):
        pass
