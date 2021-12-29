import unittest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock

from acab import setup

config = setup()
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

import acab.interfaces.handler_system as HS
from acab.error.handler import AcabHandlerException


class BasicHandlerSystem(HS.HandlerSystem_i):

    def __call__(self, *args):
        pass

    def extend(self, *args):
        pass

class TestHandlerSystem(unittest.TestCase):

    def test_creation(self):
        basic = BasicHandlerSystem()
        self.assertIsInstance(basic, HS.HandlerSystem_i)

    def test_basic_spec(self):
        spec = HS.HandlerSpec("a_signal")
        self.assertIsInstance(spec, HS.HandlerSpec)
        self.assertEqual(spec.signal, "a_signal")

    def test_register_spec(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")

        self.assertEqual(len(basic), 1)
        basic.register(spec)
        self.assertEqual(len(basic), 2)
        self.assertIn("a_signal", basic)

    def test_contains(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")

        self.assertFalse("a_signal" in basic)
        basic.register(spec)
        self.assertTrue("a_signal" in basic)

    def test_contains_spec(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")

        self.assertFalse(spec in basic)
        basic.register(spec)
        self.assertTrue(spec in basic)

    def test_contains_handler_signal(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        handler = spec.on(id)

        self.assertFalse(handler in basic)
        basic.register(spec)
        self.assertTrue(handler in basic)

    def test_register_handler(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        basic.register(spec)

        handler = HS.Handler("a_signal", func=id)
        self.assertFalse(bool(basic['a_signal']))
        basic.register(handler)
        self.assertTrue(bool(basic['a_signal']))
        self.assertEqual(len(basic['a_signal']), 1)

    def test_register_handler_two(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        basic.register(spec)

        handler  = HS.Handler("a_signal", func=id)
        handler2 = HS.Handler("a_signal", func=lambda x: None)
        self.assertFalse(bool(basic['a_signal']))
        basic.register(handler)
        basic.register(handler2)
        self.assertTrue(bool(basic['a_signal']))
        self.assertEqual(len(basic['a_signal']), 2)

    def test_register_override(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        handler  = HS.Handler("a_signal", func=id)
        handler2  = HS.Handler("a_signal", func=lambda x: None, flags=[HS.HandlerSpec.flag_e.OVERRIDE])

        basic.register(spec)
        basic.register(handler)
        basic.register(handler2)

        self.assertEqual(len(basic), 2)
        self.assertIn("a_signal", basic)

    def test_register_override(self):
        basic = BasicHandlerSystem()
        spec  = HS.HandlerSpec("a_signal")
        handler  = HS.Handler("a_signal", func=id)
        handler2  = HS.Handler("a_signal", func=lambda x: None, flags=[HS.HandlerSpec.flag_e.OVERRIDE])

        basic.register(spec)
        basic.register(handler)
        basic.register(handler2)

        self.assertEqual(len(basic), 2)
        self.assertIn("a_signal", basic)

    def test_spec_register_api(self):
        a_func : Callable[[int, str], int] = lambda x,y: x
        spec   = HS.HandlerSpec("a_signal", func_api=a_func)

        self.assertEqual(spec.func_api, a_func)

    def test_spec_enforce_api(self):
        a_func : Callable[[int, str], int] = lambda x,y: x
        spec    = HS.HandlerSpec("a_signal", func_api=a_func)
        handler = HS.Handler("a_signal", func=a_func)
        spec.check_api(func=a_func)

    def test_spec_enforce_api_fail(self):
        a_func      : Callable[[int, str], int] = lambda x,y: x
        second_func : Callable[[int, str, str], str] = lambda x,y,z: y
        spec    = HS.HandlerSpec("a_signal", func_api=a_func)
        handler = HS.Handler("a_signal", func=second_func)

        with self.assertRaises(AcabHandlerException):
            spec.register(handler)

    def test_spec_enforce_api_class(self):

        class ATest:
            def my_test_func(self, a, b):
                return b

        class AnotherTest:
            def not_my_test_func(self):
                return 2

        spec = HS.HandlerSpec("a_signal", func_api=ATest)
        spec.register(HS.Handler("a_signal", func=ATest()))

    def test_spec_enforce_api_class_fail(self):

        class ATest:
            def my_test_func(self, a, b):
                return b

        class AnotherTest:
            def not_my_test_func(self):
                return 2

        spec = HS.HandlerSpec("a_signal", func_api=ATest)
        with self.assertRaises(AcabHandlerException):
            spec.register(HS.Handler("a_signal", func=AnotherTest()))


    def test_spec_enforce_struct(self):
        spec    = HS.HandlerSpec("a_signal", struct_api=dict)
        handler = HS.Handler("a_signal", struct=set())

        with self.assertRaises(AcabHandlerException):
            spec.register(handler)


    def test_handler_call(self):
        handler = HS.Handler("a_signal", lambda *a, **k: 2)
        self.assertEqual(handler(), 2)

    def test_spec_call(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(lambda *args, **kwargs: 2)
        spec.register(handler)

        self.assertEqual(spec(), 2)

    def test_spec_call_multi(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(lambda *a, **k: 2)
        handler2 = spec.on(lambda *a, **k: 4)
        spec.register(handler)
        spec.register(handler2)

        self.assertEqual(spec(), 4)

    def test_system_call_explicit(self):
        basic   = BasicHandlerSystem(init_specs=[HS.HandlerSpec("a_signal")])
        handler = basic['a_signal'].on(lambda *a, **k: 2)
        basic.register(handler)

        self.assertEqual(basic['a_signal'](), 2)

    def test_lookup_default(self):
        basic = BasicHandlerSystem(init_specs=[HS.HandlerSpec("a_signal")])
        spec = basic.lookup()
        self.assertEqual(spec, basic[DEFAULT_HANDLER_SIGNAL])

    def test_lookup_not_default(self):
        basic = BasicHandlerSystem(init_specs=[HS.HandlerSpec("a_signal")])
        basic.register(basic['a_signal'].on(id))

        spec = basic.lookup("a_signal")
        self.assertEqual(spec, basic["a_signal"])

    def test_lookup_not_default_2(self):
        basic = BasicHandlerSystem(init_specs=[HS.HandlerSpec("a_signal"),
                                               HS.HandlerSpec('diff_signal')])
        basic.register(basic['a_signal'].on(id))
        basic.register(basic['diff_signal'].on(id))

        spec = basic.lookup("diff_signal")
        self.assertEqual(spec, basic["diff_signal"])
        self.assertNotEqual(spec, basic["a_signal"])

    def test_call_with_struct(self):
        spec = HS.HandlerSpec("a_signal")
        handler = spec.on(struct={})
        handler2 = spec.on(lambda *a, **k: a[0].update({'a': 5}))

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


    @unittest.skip("todo")
    def test_override(self):
        pass

    @unittest.skip("todo")
    def test_spec_limit(self):
        pass

    @unittest.skip("todo")
    def test_verify(self):
        pass

    @unittest.skip("todo")
    def test_register_fragment(self):
        pass
if __name__ == '__main__':
    unittest.main()
