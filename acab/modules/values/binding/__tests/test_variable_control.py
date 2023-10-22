from __future__ import annotations
import logging as logmod
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
import unittest
from unittest import mock
import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    from acab.modules.values.binding.variable_control import rectx
    import acab.core.defaults.value_keys as DS
    import acab.interfaces.value as VI

VF = VI.ValueFactory

class TestVarControl(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_val_rectx_no_op(self):
        val = VF.value("Test")
        mapping = {}
        self.assertFalse(bool(mapping))
        val_prime = rectx(val, ctx=mapping)
        self.assertFalse(bool(mapping))
        self.assertEqual(val, val_prime)


    def test_var_rectx(self):
        val = VF.value("Test", data={DS.BIND: True})
        mapping = {}
        self.assertFalse(bool(mapping))
        val_prime = rectx(val, ctx=mapping)
        self.assertTrue(bool(mapping))
        self.assertEqual(val.value, val_prime.value)
        self.assertNotEqual(val.name, val_prime.name)

    def test_var_rectx_consistency(self):
        val     = VF.value("Test", data={DS.BIND: True})
        val2    = VF.value("Test", data={DS.BIND: True})
        mapping = {}
        val_p   = rectx(val, ctx=mapping)
        val2_p  = rectx(val2, ctx=mapping)
        self.assertEqual(len(mapping), 1)
        self.assertEqual(val_p, val2_p)
        self.assertNotEqual(val, val_p)
        self.assertNotEqual(val2, val2_p)

    def test_var_rectx_anti_consistency(self):
        val     = VF.value("Test", data={DS.BIND: True})
        val2    = VF.value("Test", data={DS.BIND: True})
        mapping = {}
        val_p   = rectx(val, ctx=mapping)
        val2_p  = rectx(val2, ctx={})
        self.assertEqual(len(mapping), 1)
        self.assertNotEqual(val_p, val2_p)
        self.assertNotEqual(val, val_p)
        self.assertNotEqual(val2, val2_p)

    def test_rectx_with_params(self):
        val = VF.value("test", params=["a", "b", "c"])
        mapping = {}
        self.assertFalse(bool(mapping))
        val_p = rectx(val, ctx=mapping)
        self.assertTrue(bool(val_p.params))
        self.assertIn("a", mapping)
        self.assertIn("b", mapping)
        self.assertIn("c", mapping)

    def test_sen_rectx_no_op(self):
        sen     = VF.sen() << "a" << "test" << "sentence"
        mapping = {}
        val_p   = rectx(sen, ctx=mapping)
        self.assertFalse(bool(mapping))
        self.assertEqual(val_p, sen)

    def test_sen_rectx(self):
        val = VF.sen() << ["a", VF.value("test", data={DS.BIND: True}), "sentence"]
        mapping = {}
        self.assertFalse(bool(mapping))
        val_p = rectx(val, ctx=mapping)
        self.assertIn("test", mapping)

    def test_sen_rectx_with_params(self):
        val = VF.sen(params=["a", "b", "c"]) << ["a", "test", "sentence"]
        mapping = {}
        self.assertFalse(bool(mapping))
        val_p = rectx(val, ctx=mapping)
        self.assertTrue(bool(val_p.params))
        self.assertIn("a", mapping)
        self.assertIn("b", mapping)
        self.assertIn("c", mapping)

    def test_sen_rectx_with_multi_var_coherence(self):
        sen = VF.sen() << "a" << VF.value("test", data={DS.BIND: True}) << VF.value("test", data={DS.BIND: True})
        mapping = {}
        val_p = rectx(sen, ctx=mapping)
        self.assertTrue(bool(mapping))
        self.assertIn("test", mapping)
        self.assertEqual(val_p[-1], val_p[-2])

    def test_multi_sen_rectx_with_coherence(self):
        sen     = VF.sen() << "a" << VF.value("test", data={DS.BIND: True})
        sen2    = VF.sen() << "diff" << VF.value("test", data={DS.BIND: True})
        mapping = {}
        val_p   = rectx(sen, ctx=mapping)
        val2_p  = rectx(sen2, ctx=mapping)
        self.assertEqual(val_p[-1], val2_p[-1])
        self.assertIn("test", mapping)

    def test_multi_sen_rectx_with_anti_coherence(self):
        sen     = VF.sen() << "a" << VF.value("test", data={DS.BIND: True})
        sen2    = VF.sen() << "diff" << VF.value("test", data={DS.BIND: True})
        mapping = {}
        val_p   = rectx(sen, ctx=mapping)
        val2_p  = rectx(sen2, ctx={})
        self.assertNotEqual(val_p[-1], val2_p[-1])
        self.assertIn("test", mapping)

    def test_sen_rectx_with_multi_var_rename(self):
        sen = VF.sen() << "a" << VF.value("test", data={DS.BIND: True}) << VF.value("sentence", data={DS.BIND: True})
        mapping = {}
        val_p = rectx(sen, ctx=mapping)
        self.assertIn("test", mapping)
        self.assertIn("sentence", mapping)
        self.assertNotEqual(sen, val_p)
        self.assertNotEqual(sen[-1], val_p[1])
        self.assertNotEqual(sen[-2], val_p[2])

    def test_sen_rectx_effects_types(self):
        type_sen = VF.sen() << VF.value("a", data={DS.BIND: True})
        sen = VF.sen() << "a" << "test" << VF.value("sentence", data={DS.TYPE_INSTANCE: type_sen})

        mapping = {}
        sen_p = rectx(sen, ctx=mapping)
        self.assertIn("a", mapping)


##-- ifmain
if __name__ == '__main__':
    unittest.main()
##-- end ifmain
