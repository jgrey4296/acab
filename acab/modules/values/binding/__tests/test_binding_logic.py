#!/usr/bin/env python3
"""

"""
import logging as logmod
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import acab
import pyparsing as pp
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    import acab.interfaces.value as VI
    from acab.core.parsing import pyparse_dsl as ppDSL
    import acab.core.defaults.value_keys as DS
    from acab.modules.context.context_instance import ContextInstance
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.core.data.node import AcabNode
    from acab.modules.values.binding.binding import Bind
    from acab.modules.values.sen_val.module import Sen_Val_Parser


class BindingLogicTests(unittest.TestCase):

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

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Sen_Val_Parser)
        cls.dsl.build()
        # dsl()


    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def test_no_change(self):
        source = self.dsl("test")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance()

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertIs(source, result)


    def test_no_change_var(self):
        source = self.dsl("$x")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance()

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertIs(source, result)

    def test_no_change_var_mismatch(self):
        source = self.dsl("$x")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        target = self.dsl("target")[0][0]
        self.assertIsInstance(target, VI.Value_i)
        ctx = ContextInstance({"y": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertEqual(source, result)

    def test_basic_word(self):
        source = self.dsl("$x")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        target = self.dsl("target")[0][0]
        self.assertIsInstance(target, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(target, result)

    def test_basic_sentence_no_change(self):
        source = self.dsl("a.test.sentence")[0]
        self.assertIsInstance(source, VI.Sentence_i)
        ctx = ContextInstance()

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Sentence_i)
        self.assertEqual(source, result)

    def test_basic_sentence_simple_var_sub_word(self):
        source = self.dsl("a.test.$x")[0]
        target = self.dsl("sentence")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence")

    def test_basic_sentence_simple_var_sub_sen(self):
        source = self.dsl("a.test.$x")[0]
        target = self.dsl("sentence")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.[sentence]")


    def test_basic_sentence_repeat_sub(self):
        source = self.dsl("a.test.$x.$x")[0]
        target = self.dsl("sentence")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.sentence")

    def test_basic_sentence_multi_sub(self):
        source = self.dsl("a.test.$x.$y")[0]
        target = self.dsl("sentence")[0][0]
        target2 = self.dsl("blah")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target, "y": target2})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.blah")

    def test_basic_sentence_sub_sentence_default_sharp(self):
        source = self.dsl("a.test.$x")[0]
        target = self.dsl("sentence.blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.[sentence.blah]")
        self.assertIsInstance(result[-1], VI.Value_i)
        self.assertEqual(result[-1][-1], "blah")

    def test_basic_sentence_sub_sentence_explicit_flatten(self):
        source = self.dsl("a.test.$x(♭)")[0]
        target = self.dsl("sentence.blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.blah")
        self.assertEqual(len(result), 4)

    def test_basic_sentence_sub_sentence_explicit_sharp(self):
        source = self.dsl("a.test.$x(♯)")[0]
        target = self.dsl("sentence.blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})
        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.[sentence.blah]")
        self.assertEqual(len(result), 3)
        self.assertIsInstance(result[-1], VI.Sentence_i)

    def test_at_var_binding(self):
        source = self.dsl("@x")[0]
        target = self.dsl("val")[0][0]
        target_node = AcabNode(target)

        ctx = ContextInstance({"x": target}, nodes={"x": target_node})
        result = Bind.bind(source[0], ctx)
        self.assertIsInstance(result, AcabNode)

    def test_at_var_in_sentence(self):
        source = self.dsl("@x.b.c")[0]
        target = self.dsl("val")[0][0]
        target_node = AcabNode(target)
        ctx = ContextInstance({"x": target}, nodes={"x": target_node})

        result = Bind.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertEqual(result, "_:val.b.c")
        self.assertIsInstance(result[0], VI.Value_i)
        self.assertNotIsInstance(result[0], VI.Sentence_i)

    def test_container_bind(self):
        source = self.dsl['sentence.ends'].parse_string("test(::γ):\n a.b.$x?\nend")[0]

        ctx = ContextInstance({'x': self.dsl("val")[0][0]})
        result = Bind.bind(source, ctx)

        self.assertEqual(result.type[:2], "_:INSTRUCT.CONTAINER")
        self.assertEqual(result[0], "_:a.b.val")


    def test_container_bind_ignore_params(self):
        source = self.dsl['sentence.ends'].parse_string("test(::γ):\n | $x |\n\n a.b.$x?\nend")[0]

        ctx = ContextInstance({'x': self.dsl("val")[0]})
        result = Bind.bind(source, ctx)

        self.assertEqual(result.type[:2], "_:INSTRUCT.CONTAINER")
        self.assertEqual(result[0], "_:a.b.x")

    def test_sen_bind_container(self):
        source = self.dsl("a.container.test(::γ):\n a.b.$x?\nend")[0]

        ctx = ContextInstance({'x': self.dsl("val")[0][0]})

        result = Bind.bind(source, ctx)

        self.assertEqual(result.type[:2], "_:SENTENCE")
        self.assertEqual(result, "_:a.container.'test'")
        self.assertEqual(result[-1][0], "_:a.b.val")

    def test_sen_bind_container_param_ignore(self):
        source = self.dsl("a.container.test(::γ):\n | $x |\n\n a.b.$x?\nend")[0]

        ctx = ContextInstance({'x': self.dsl("val")[0]})
        result = Bind.bind(source, ctx)

        self.assertEqual(result.type[:2], "_:SENTENCE")
        self.assertEqual(result, "_:a.container.'test'")
        self.assertEqual(result[-1][0], "_:a.b.x")

    def test_sen_bind_container_in_container(self):
        source = self.dsl("a.container.test(::α):\n !! a.b.$x\n !! another.container(::γ):\n  a.b.$x?\n end\nend")[0]
        ctx = ContextInstance({'x': self.dsl("val")[0][0]})
        result = Bind.bind(source, ctx)
        self.assertEqual(result.type[:2], "_:SENTENCE")
        self.assertEqual(result, "_:a.container.'test'")
        self.assertEqual(result[-1][0], "_:[!!].[[a.b.val]].[returns.unit]")
        self.assertEqual(result[-1][1], "_:[!!].[[another.'container']].[returns.unit]")
        self.assertEqual(result[-1][1][1][0][-1].type[:2], "_:INSTRUCT.CONTAINER")
        self.assertEqual(result[-1][1][1][0][-1][0], "_:a.b.val")

    def test_sen_bind_container_in_container_param_ignore(self):
        source = self.dsl("a.container.test(::α):\n !! a.b.$x\n !! another.container(::γ):\n  | $x |\n\n  a.b.$x?\n end\nend")[0]
        ctx = ContextInstance({'x': self.dsl("val")[0]})
        result = Bind.bind(source, ctx)

        self.assertEqual(result.type[:2], "_:SENTENCE")
        self.assertEqual(result, "_:a.container.'test'")
        self.assertEqual(result[-1][0], "_:[!!].[[a.b.val]].[returns.unit]")
        self.assertEqual(result[-1][1], "_:[!!].[[another.'container']].[returns.unit]")
        self.assertEqual(result[-1][1][1][0][-1].type[:2], "_:INSTRUCT.CONTAINER")
        self.assertEqual(result[-1][1][1][0][-1][0], "_:a.b.x")

    def test_sen_bind_container_in_container_param_ignore(self):
        source = self.dsl("a.container.test(::α):\n | $x |\n\n !! a.b.$x\n !! another.container(::γ):\n  a.b.$x?\n end\nend")[0]
        ctx = ContextInstance({'x': self.dsl("val")[0]})
        result = Bind.bind(source, ctx)

        self.assertEqual(result.type[:2], "_:SENTENCE")
        self.assertEqual(result, "_:a.container.'test'")
        self.assertEqual(result[-1][0], "_:[!!].[[a.b.x]].[returns.unit]")
        self.assertEqual(result[-1][1], "_:[!!].[[another.'container']].[returns.unit]")
        self.assertEqual(result[-1][1][1][0][-1].type[:2], "_:INSTRUCT.CONTAINER")
        self.assertEqual(result[-1][1][1][0][-1][0], "_:a.b.x")


    def test_solo_var_sen_bind(self):
        sen1 = self.dsl("$x")[0]
        ctx = ContextInstance({"x": self.dsl("a.b.c")[0]})
        result = Bind.bind(sen1, ctx)
        self.assertEqual(result, "_:a.b.c")
