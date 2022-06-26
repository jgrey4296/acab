import logging as logmod
import unittest
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeVar,
                    cast)
from unittest import mock

logging = logmod.getLogger(__name__)

import warnings

import acab
from acab.interfaces.handler_system import Handler_i

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()
HandlerConfigSpec      = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": Handler_i})
config.override(HandlerConfigSpec, "acab.core.util.part_implementations.handler.BasicHandler")

Handler = HandlerConfigSpec()

import acab.core.util.part_implementations.handler_system as HS
import acab.interfaces.handler_system as HSi
from acab.error.handler import AcabHandlerException, HandlerDuplicationException


class SimplestHandlerSystem(HS.HandlerSystem):

    def __call__(self, *args):
        pass

    def extend(self, *args):
        pass

class TestHandlerSystem(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def test_creation(self):
        basic = SimplestHandlerSystem()
        self.assertIsInstance(basic, HSi.HandlerSystem_i)

    def test_creation_keywords(self):
        basic = SimplestHandlerSystem(init_specs=[], init_handlers=[], sieve_fns=[])
        self.assertIsInstance(basic, HSi.HandlerSystem_i)

    def test_creation_keywords(self):
        basic = SimplestHandlerSystem(init_specs=[], init_handlers=[])
        self.assertIsInstance(basic, HSi.HandlerSystem_i)

    def test_register_spec(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")

        self.assertEqual(len(basic), 1)
        basic.register(spec)
        self.assertEqual(len(basic), 2)
        self.assertIn("a_signal", basic)

    def test_contains(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")

        self.assertFalse("a_signal" in basic)
        basic.register(spec)
        self.assertTrue("a_signal" in basic)

    def test_contains_spec(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")

        self.assertFalse(spec in basic)
        basic.register(spec)
        self.assertTrue(spec in basic)

    def test_contains_handler_signal(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        handler = spec.on(target=id)

        self.assertFalse(handler in basic)
        basic.register(spec)
        self.assertTrue(handler in basic)

    def test_register_handler(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        basic.register(spec)

        handler = Handler("a_signal", func=id)
        self.assertFalse(bool(basic['a_signal']))
        basic.register(handler)
        self.assertTrue(bool(basic['a_signal']))
        self.assertEqual(len(basic['a_signal']), 1)

    def test_register_handler_two(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        basic.register(spec)

        handler  = Handler("a_signal", func=id)
        handler2 = Handler("a_signal", func=lambda x: None)
        self.assertFalse(bool(basic['a_signal']))
        basic.register(handler)
        basic.register(handler2)
        self.assertTrue(bool(basic['a_signal']))
        self.assertEqual(len(basic['a_signal']), 2)

    def test_register_handler_duplicate(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        basic.register(spec)

        handler  = Handler("a_signal", func=id)
        handler2 = Handler("a_signal", func=id)
        self.assertFalse(bool(basic['a_signal']))
        basic.register(handler)
        with self.assertRaises(HandlerDuplicationException):
            basic.register(handler2)

        self.assertTrue(bool(basic['a_signal']))
        self.assertEqual(len(basic['a_signal']), 1)

    def test_register_override(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        handler  = Handler("a_signal", func=id)
        handler2  = Handler("a_signal", func=lambda x: None, flags=[HS.HandlerSpec.flag_e.OVERRIDE])

        basic.register(spec)
        basic.register(handler)
        basic.register(handler2)

        self.assertEqual(len(basic), 2)
        self.assertIn("a_signal", basic)

    def test_register_override_2(self):
        basic = SimplestHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        handler  = Handler("a_signal", func=id)
        handler2  = Handler("a_signal", func=lambda x: None, flags=[HS.HandlerSpec.flag_e.OVERRIDE])

        basic.register(spec)
        basic.register(handler)
        basic.register(handler2)

        self.assertEqual(len(basic), 2)
        self.assertIn("a_signal", basic)

    def test_handler_call(self):
        handler = Handler("a_signal", func=lambda *a, **k: 2)
        self.assertEqual(handler(), 2)

    def test_system_call_explicit(self):
        basic   = SimplestHandlerSystem(init_specs=[HS.HandlerSpec("a_signal")])
        handler = basic['a_signal'].on(target=lambda *a, **k: 2)
        basic.register(handler)

        self.assertEqual(basic['a_signal'](), 2)

    def test_lookup_default(self):
        basic = SimplestHandlerSystem(init_specs=[HS.HandlerSpec("a_signal")])
        spec = basic.lookup()
        self.assertEqual(spec, basic[DEFAULT_HANDLER_SIGNAL])

    def test_lookup_not_default(self):
        basic = SimplestHandlerSystem(init_specs=[HS.HandlerSpec("a_signal")])
        basic.register(basic['a_signal'].on(target=id))

        spec = basic.lookup("a_signal")
        self.assertEqual(spec, basic["a_signal"])

    def test_lookup_not_default_2(self):
        basic = SimplestHandlerSystem(init_specs=[HS.HandlerSpec("a_signal"), HS.HandlerSpec('diff_signal')])
        basic.register(basic['a_signal'].on(target=id))
        basic.register(basic['diff_signal'].on(target=id))

        spec = basic.lookup("diff_signal")
        self.assertEqual(spec, basic["diff_signal"])
        self.assertNotEqual(spec, basic["a_signal"])


if __name__ == '__main__':
    unittest.main()
