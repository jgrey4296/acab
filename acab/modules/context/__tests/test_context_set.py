from __future__ import annotations

import logging as logmod
import unittest
import warnings
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from unittest import mock
import warnings

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    pass


import acab
from acab.core.value.value import AcabValue
from acab.error.context import AcabContextException
from acab.modules.context.context_instance import ContextInstance
from acab.modules.context.context_set import ContextSet


class ContextsTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        if bool(logmod.root.handlers):
            logmod.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    #----------
    def test_set_basic(self):
        """ Check a ContextSet can be created """
        ctx = ContextSet()
        self.assertIsNotNone(ctx)
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 1)

    def test_set_append(self):
        """ Check a context set can manipulate its contents """
        ctx = ContextSet()
        self.assertTrue(bool(ctx))
        ctx.pop()
        self.assertFalse(bool(ctx))
        self.assertEqual(len(ctx), 0)
        ctx.push(ContextInstance())
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 1)
        ctx.push(ContextInstance())
        self.assertEqual(len(ctx), 2)

    def test_set_append_2(self):
        """ Check multiple context instances can be pushed onto a context set """
        ctx = ContextSet()
        ctx.pop()
        self.assertFalse(bool(ctx))
        ctx.push([ContextInstance(),
                  ContextInstance(),
                  ContextInstance()])
        self.assertTrue(bool(ctx))
        self.assertEqual(len(ctx), 3)

    def test_set_fail(self):
        """ Check context instances can be marked failed in the context set """
        ctx = ContextSet()
        inst = ctx.pop()
        self.assertFalse(bool(ctx))
        ctx.fail(inst, "fail word", "fail_node")
        self.assertFalse(bool(ctx))
        self.assertEqual(inst._failure_word, "fail word")
        self.assertIn(inst.uuid, ctx._purgatory)

    def test_set_iteration(self):
        """ Check context instances can be iterated via the context set """
        ctx = ContextSet()
        ctx.pop()
        ctx.push([ContextInstance({"a" : 1}),
                  ContextInstance({"b" : 2}),
                  ContextInstance({"c" : 3})])

        for x,y in zip(ctx, ['a','b','c']):
            self.assertTrue(y in x.data)


    def test_set_alts_binding(self):
        """ Check context instances can set their current focus node """
        ctx = ContextSet()
        ctx.pop()
        ctx.push([ContextInstance(nodes={'a': "blah"}),
                  ContextInstance(nodes={'a': "bloo"}),
                  ContextInstance(nodes={'a': "blee"})])

        [x.set_current_binding(AcabValue("a")) for x in ctx.active_list()]
        self.assertEqual(len(ctx.active_list()), 3)
        for x,y in zip(ctx.active_list(), ["blah","bloo","blee"]):
            self.assertEqual(x._current, y)

    def test_set_alts_invalid_binding(self):
        """ Check context instances can't focus on non-existent nodes """
        ctx = ContextSet()
        ctx.pop()
        ctx.push([ContextInstance(nodes={'a': "blah"}),
                  ContextInstance(nodes={'b': "bloo"}),
                  ContextInstance(nodes={'a': "blee"})])
        with self.assertRaises(AcabContextException):
            [x.set_current_binding(AcabValue("a")) for x in ctx.active_list()]

    def test_set_enter_exit(self):
        pass

    def test_set_fail(self):
        pass

    def test_set_pop(self):
        pass

    def test_set_test(self):
        pass


    def test_subctx(self):
        pass

    def test_push(self):
        pass

    def test_active_list(self):
        pass

    def test_set_parent(self):
        pass

    def test_named_set(self):
        pass
