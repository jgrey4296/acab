#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.core.util.fragments import DSL_Fragment
    from acab.error.parse import AcabParseException
    from acab.interfaces import dsl as DSLi
    from acab.interfaces import handler_system as hi

DEFAULT_HANDLER_SIGNAL = config.attr.Handler.System.DEFAULT_SIGNAL

class PyParseDSLTests(unittest.TestCase):

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

    def test_init(self):
        self.assertIsInstance(ppDSL.PyParseDSL(), DSLi.DSL_Builder_i)
        self.assertIsInstance(ppDSL.PyParseDSL(), hi.HandlerSystem_i)

    def test_spec_init(self):
        a_forward = pp.Forward()
        spec = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])

        self.assertIsInstance(spec, DSLi.DSL_Spec_i)
        self.assertIsInstance(spec, hi.HandlerSpec_i)
        self.assertEqual(spec.signal, "a_signal")

    def test_handler_init(self):
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))
        self.assertIsInstance(handler, DSLi.DSL_Handler_i)
        self.assertIsInstance(handler, hi.Handler_i)
        self.assertEqual(handler.signal, "a_signal")

    def test_handler_init_fail(self):
        with self.assertRaises(AcabParseException):
            ppDSL.PyParse_Handler("a_signal", func="blah")

    def test_register_handler_into_spec(self):
        a_forward = pp.Forward()
        spec = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))

        self.assertFalse(bool(spec))
        spec.register(handler)
        self.assertTrue(bool(spec))


    def test_register_multi_handler_into_spec(self):
        a_forward = pp.Forward()
        spec = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))
        handler2 = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("aweg"))

        self.assertEqual(len(spec), 0)
        spec.register(handler)
        self.assertEqual(len(spec), 1)
        spec.register(handler2)
        self.assertEqual(len(spec), 2)


    def test_spec_build(self):
        a_forward = pp.Forward()
        spec = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))
        spec.register(handler)

        self.assertEqual(a_forward.expr, None)
        spec.build()
        self.assertNotEqual(a_forward.expr, None)


    def test_spec_build_multi(self):
        a_forward = pp.Forward()
        spec = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))
        handler2 = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("aweg"))
        spec.register(handler)
        spec.register(handler2)

        spec.build()
        result = spec.parse_string("blah")
        result2 = spec.parse_string("aweg")
        self.assertEqual(result[0], "blah")
        self.assertEqual(result2[0], "aweg")

    def test_spec_extend(self):
        a_forward = pp.Forward()
        b_forward = pp.Forward()

        spec1 = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        spec2 = ppDSL.PyParse_Spec("a_signal", struct=b_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])

        self.assertEqual(len(spec1.struct), 1)
        spec1.extend_spec(spec2)
        self.assertEqual(len(spec1.struct), 2)

    def test_spec_extended_build(self):
        a_forward = pp.Forward()
        b_forward = pp.Forward()

        spec1 = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        spec2 = ppDSL.PyParse_Spec("a_signal", struct=b_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))
        spec1.register(handler)
        spec1.extend_spec(spec2)

        self.assertEqual(a_forward.expr, None)
        self.assertEqual(b_forward.expr, None)
        spec1.build()
        self.assertNotEqual(a_forward.expr, None)
        self.assertNotEqual(b_forward.expr, None)
        self.assertEqual(a_forward.expr, b_forward.expr)

    def test_dsl_register_spec(self):
        dsl       = ppDSL.PyParseDSL()
        a_forward = pp.Forward()
        spec1     = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])

        self.assertFalse(dsl)
        dsl.register(spec1)
        self.assertTrue(dsl)

    def test_dsl_extend_spec(self):
        dsl       = ppDSL.PyParseDSL()
        a_forward = pp.Forward()
        spec1     = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        b_forward = pp.Forward()
        spec2     = ppDSL.PyParse_Spec("a_signal", struct=b_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])

        dsl.register(spec1)
        self.assertEqual(len(spec1.struct), 1)
        dsl.register(spec2)
        self.assertEqual(len(spec1.struct), 2)

    def test_dsl_register_loose(self):
        dsl     = ppDSL.PyParseDSL()
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))

        self.assertEqual(len(dsl.loose_handlers), 0)
        dsl.register(handler)
        self.assertEqual(len(dsl.loose_handlers), 1)

    def test_dsl_build_consumes_loose_handlers(self):
        dsl     = ppDSL.PyParseDSL()
        handler = ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah"))
        a_forward = pp.Forward()
        spec1     = ppDSL.PyParse_Spec("a_signal", struct=a_forward, flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])

        dsl.register(handler)
        dsl.register(spec1)
        self.assertEqual(len(dsl.loose_handlers), 1)
        dsl.build()
        self.assertEqual(len(dsl.loose_handlers), 0)

    def test_dsl_register_fragment_specs(self):
        dsl      = ppDSL.PyParseDSL()
        fragment = DSL_Fragment(specs=[
            ppDSL.PyParse_Spec("a_signal", struct=pp.Forward(), flags=[ppDSL.PyParse_Spec.flag_e.COLLECT]),
            ppDSL.PyParse_Spec("b_signal", struct=pp.Forward(), flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
        ])

        # 1 for default
        self.assertEqual(len(dsl), 1)
        dsl.register(fragment)
        self.assertEqual(len(dsl), 3)
        self.assertEqual(len(dsl['a_signal']), 0)
        self.assertEqual(len(dsl['b_signal']), 0)

    def test_dsl_register_fragment_handlers(self):
        dsl      = ppDSL.PyParseDSL()
        fragment = DSL_Fragment(specs=[
            ppDSL.PyParse_Spec("a_signal", struct=pp.Forward(), flags=[ppDSL.PyParse_Spec.flag_e.COLLECT]),
            ppDSL.PyParse_Spec("b_signal", struct=pp.Forward(), flags=[ppDSL.PyParse_Spec.flag_e.COLLECT])
            ],
                                      handlers=[
                                          ppDSL.PyParse_Handler("a_signal", func=pp.Literal("blah")),
                                          ppDSL.PyParse_Handler("b_signal", func=pp.Literal("blah"))
                                      ])

        dsl.register(fragment)
        self.assertEqual(len(dsl['a_signal']), 1)
        self.assertEqual(len(dsl['b_signal']), 1)

    def test_dsl_parse_failure(self):
        dsl = ppDSL.PyParseDSL()
        dsl.build()
        with self.assertRaises(AcabParseException):
            dsl.parse("blah")

    def test_dsl_parse_default(self):
        dsl = ppDSL.PyParseDSL(init_handlers=[ppDSL.PyParse_Handler(DEFAULT_HANDLER_SIGNAL, func=pp.Literal("blah"))])

        dsl.build()
        dsl.parse("blah")
