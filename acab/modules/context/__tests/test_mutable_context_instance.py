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
    from acab.modules.context.context_instance import ContextInstance, MutableContextInstance
    from acab.modules.context.context_set import ContextSet
    from acab.core.data.node import AcabNode
    from acab.interfaces.value import ValueFactory as VF
    from acab.error.context import AcabContextException


class TestMutableContextInstance(unittest.TestCase):

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

    def test_mutable_requires_parent(self):
        with self.assertRaises(AcabContextException) as cm:
            mutx = MutableContextInstance(None, parent_set={})

        self.assertEqual(cm.exception.detail, "Mutable Context must have a context set as parent")

    def test_mutable_requires_base(self):
        with self.assertRaises(AcabContextException) as cm:
            mutx = MutableContextInstance(None)

        self.assertEqual(cm.exception.detail, "Mutable Context must have a context as base")


    def test_mutable_basic(self):
        """ Check a MutableContextInstance can be created """
        inst = ContextInstance()
        mutx = MutableContextInstance(inst)
        self.assertIsInstance(mutx, MutableContextInstance)
        self.assertEqual(mutx.base.uuid, inst.uuid)

    def test_mutable_bindings(self):
        """ Check a MutableContextInstance can be created with bindings """
        mutx = MutableContextInstance(ContextInstance(), {"a" : 2, "b" : 3})
        self.assertEqual(mutx.data['a'], 2)
        self.assertEqual(mutx.data['b'], 3)

    def test_mutable_copy(self):
        """ Check a MutableContextInstance be copied,
        and produces a *ContextInstance*
        """
        mutx = MutableContextInstance(ContextInstance(), data={"a": 2, "b": 3})
        ctx  = mutx.copy()
        self.assertNotEqual(id(mutx), id(ctx))
        self.assertNotEqual(mutx.uuid, ctx.uuid)
        self.assertIsInstance(ctx, ContextInstance)

    def test_mutable_nesting(self):
        mutx  = MutableContextInstance(ContextInstance(), data={"a": 2, "b": 3})
        mutx2 = MutableContextInstance(mutx, data={"a": 5, "c": 3})

        self.assertIsInstance(mutx2, MutableContextInstance)
        self.assertEqual(mutx2.base.uuid, mutx.uuid)
        self.assertIn("a", mutx2)
        self.assertIn("b", mutx2)
        self.assertIn("c", mutx2)

        self.assertEqual(mutx2.data['a'], 5)

    def test_mutable_nested_independence(self):
        """ Check a context instance can be copied """
        mutx  = MutableContextInstance(ContextInstance(), data={"a": 2, "b": 3})
        mutx2 = MutableContextInstance(mutx, data={"a": 5, "c": 3})

        self.assertEqual(mutx.data['a'], 2)
        self.assertEqual(mutx2.data['a'], 5)

        mutx2.data['q'] = True
        self.assertIn('q', mutx2)
        self.assertNotIn('q', mutx)


    def test_is_mutable(self):
        mutx  = MutableContextInstance(ContextInstance(), data={"a": 2, "b": 3})
        self.assertEqual(mutx['a'], 2)
        mutx['a'] = "blah"
        self.assertEqual(mutx['a'], "blah")


    def test_empty(self):
        mutx  = MutableContextInstance(ContextInstance())
        self.assertFalse(mutx)

    def test_not_empty(self):
        mutx  = MutableContextInstance(ContextInstance())
        self.assertFalse(mutx)
        mutx['a'] = 5
        self.assertTrue(mutx)

    def test_not_empty_base(self):
        inst = ContextInstance({"a": VF.value("blah")})
        mutx  = MutableContextInstance(inst)
        self.assertTrue(mutx)


    def test_mutable_contains_base(self):
        inst = ContextInstance({"a": VF.value("blah")})
        mutx  = MutableContextInstance(inst)
        self.assertIn("a", mutx)

    def test_mutable_contains_base_fail(self):
        inst = ContextInstance({"a": VF.value("blah")})
        mutx  = MutableContextInstance(inst)
        self.assertNotIn("b", mutx)

    def test_mutable_contains(self):
        inst = ContextInstance()
        mutx  = MutableContextInstance(inst, data={"a", 5})
        self.assertIn("a", mutx)

    def test_mutable_contains_fail(self):
        inst = ContextInstance()
        mutx  = MutableContextInstance(inst)
        self.assertNotIn("b", mutx)

    def test_mutable_dict_progress(self):
        inst = ContextInstance({"a": VF.value("blah")})
        mutx  = MutableContextInstance(inst)
        result = mutx.progress({"b": VF.value("bloo")}, {})

        self.assertIs(mutx, result[0])
        self.assertIn("b", mutx)

    def test_mutable_dict_progress_multi(self):
        inst   = ContextInstance({"a": VF.value("blah")})
        mutx   = MutableContextInstance(inst)
        result = mutx.progress({"b": VF.value("bloo"),
                                "c" : VF.value("aweg")}, {})
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 1)
        self.assertIs(mutx, result[0])
        self.assertIn("b", mutx)
        self.assertIn("c", mutx)

    def test_mutable_val_progress_fail(self):
        inst = ContextInstance({"a": VF.value("blah")})
        mutx  = MutableContextInstance(inst)

        with self.assertRaises(AcabContextException) as cm:
            mutx.progress(VF.value("b"), [AcabNode(VF.value("c"))])

        self.assertEqual(cm.exception.detail, "Progressing a MuCtxInst only handles dict binding")


    def test_context_manager(self):
        inst = ContextInstance({"a": VF.value("blah")})
        mutx  = MutableContextInstance(inst)

        self.assertIsNone(mutx._finished)
        with mutx:
            mutx['b'] = 5

        self.assertIsNotNone(mutx._finished)
        final = mutx.final_ctx
        self.assertIsInstance(final, ContextInstance)
        self.assertIn("b", final)

    def test_context_manager_2(self):
        inst = ContextInstance({"a": VF.value("blah")})
        with MutableContextInstance(inst) as mutx:
            mutx['b'] = 5

        self.assertIsNotNone(mutx._finished)
        final = mutx.final_ctx
        self.assertIsInstance(final, ContextInstance)
        self.assertIn("b", final)
