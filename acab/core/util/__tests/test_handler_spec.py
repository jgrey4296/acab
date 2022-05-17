import logging as logmod
import unittest
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeVar,
                    cast)
from unittest import mock

logging = logmod.getLogger(__name__)

import warnings

from acab import setup
from acab.interfaces.handler_system import Handler_i

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config                 = setup()

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()
HandlerConfigSpec      = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": Handler_i})
config.override(HandlerConfigSpec, "acab.core.util.part_implementations.handler.BasicHandler")

Handler = HandlerConfigSpec()

import acab.core.util.part_implementations.handler_system as HS
import acab.interfaces.handler_system as HSi
from acab.error.handler import (AcabHandlerException,
                                HandlerDuplicationException)


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

    def test_basic_spec(self):
        spec = HS.HandlerSpec("a_signal")
        self.assertIsInstance(spec, HSi.HandlerSpec_i)
        self.assertEqual(spec.signal, "a_signal")

    def test_spec_register_api(self):
        a_func : Callable[[int, str], int] = lambda x,y: x
        spec   = HS.HandlerSpec("a_signal", func_api=a_func)

        self.assertEqual(spec.func_api, a_func)

    def test_spec_enforce_api(self):
        a_func : Callable[[int, str], int] = lambda x,y: x
        spec    = HS.HandlerSpec("a_signal", func_api=a_func)
        handler = Handler("a_signal", func=a_func)
        spec.check_api(func=a_func)

    def test_spec_enforce_api_fail(self):
        a_func      : Callable[[int, str], int] = lambda x,y: x
        second_func : Callable[[int, str, str], str] = lambda x,y,z: y
        spec    = HS.HandlerSpec("a_signal", func_api=a_func)
        handler = Handler("a_signal", func=second_func)

        with self.assertRaises(AcabHandlerException):
            spec.register(handler)

    def test_spec_enforce_api_class(self):

        class ATest:
            def __call__(self, a, b):
                return b

            def other(self):
                pass

        spec = HS.HandlerSpec("a_signal", func_api=ATest)
        spec.register(Handler("a_signal", func=ATest()))

    def test_spec_enforce_api_class_fail(self):

        class ATest:
            def __call__(self, a, b):
                return b

            def other(self):
                pass

        class AnotherTest:
            def __call__(self):
                return 2

        spec    = HS.HandlerSpec("a_signal", func_api=ATest)
        handler = Handler("a_signal", func=AnotherTest())
        with self.assertRaises(AcabHandlerException):
            spec.register(handler)


    def test_spec_enforce_struct(self):
        spec    = HS.HandlerSpec("a_signal", struct_api=dict)
        handler = Handler("a_signal", struct=set())

        with self.assertRaises(AcabHandlerException):
            spec.register(handler)

    def test_spec_register_twice(self):
        def test_func(val):
            pass

        spec = HS.HandlerSpec("a_signal")
        handler = Handler("a_signal", func=test_func)

        spec.register(handler)
        with self.assertRaises(HandlerDuplicationException):
            spec.register(handler)

    def test_spec_register_twice_sep_handlers(self):
        def test_func(val):
            pass

        spec = HS.HandlerSpec("a_signal")
        handler = Handler("a_signal", func=test_func)
        handler2 = Handler("a_signal", func=test_func)

        spec.register(handler)
        with self.assertRaises(HandlerDuplicationException):
            spec.register(handler2)

    def test_spec_register_twice_from_class(self):
        class TestFuncClass:
            def test_func(val):
                pass

        instance = TestFuncClass()
        spec = HS.HandlerSpec("a_signal")
        handler = Handler("a_signal", func=instance.test_func)
        handler2 = Handler("a_signal", func=instance.test_func)

        spec.register(handler)
        with self.assertRaises(HandlerDuplicationException):
            spec.register(handler2)

    def test_spec_register_twice_from_callable_dataclass(self):
        @dataclass
        class TestFuncClass:
            def __call__(val):
                pass

        instance = TestFuncClass()
        spec = HS.HandlerSpec("a_signal")
        handler = Handler("a_signal", func=instance)
        handler2 = Handler("a_signal", func=instance)

        spec.register(handler)
        with self.assertRaises(HandlerDuplicationException):
            spec.register(handler2)


    def test_spec_register_twice_from_different_class_instances(self):
        class TestFuncClass:
            def test_func(val):
                pass

        instance = TestFuncClass()
        instance2 = TestFuncClass()
        spec = HS.HandlerSpec("a_signal")
        handler = Handler("a_signal", func=instance.test_func)
        handler2 = Handler("a_signal", func=instance2.test_func)

        spec.register(handler)
        spec.register(handler2)
        self.assertEqual(len(spec), 2)


    def test_spec_call(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(target=lambda *args, **kwargs: 2)
        spec.register(handler)

        self.assertEqual(spec(), 2)

    def test_spec_call_multi(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(target=lambda *a, **k: 2)
        handler2 = spec.on(target=lambda *a, **k: 4)
        spec.register(handler)
        spec.register(handler2)

        self.assertEqual(spec(), 4)

    def test_call_with_struct(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(struct={})
        handler2 = spec.on(target=lambda *a, **k: a[0].update({'a': 5}))

        spec.register(handler)
        spec.register(handler2)

        self.assertFalse(spec.struct)
        spec()
        self.assertIn('a', spec.struct)
        self.assertEqual(spec.struct['a'], 5)

    def test_multi_struct_fail(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(struct={})
        handler2 = spec.on(struct=set())

        spec.register(handler)
        with self.assertRaises(AcabHandlerException):
            spec.register(handler2)

    def test_multi_struct_override(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(struct={})
        handler2 = spec.on(struct=set(), flags=[HS.HandlerSpec.flag_e.OVERRIDE])

        spec.register(handler)
        spec.register(handler2)
        self.assertIsInstance(spec.struct, set)


if __name__ == '__main__':
    unittest.main()
