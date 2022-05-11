#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as logmod
logging = logmod.getLogger(__name__)

import random

import acab
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()

from acab.core.value.value import AcabValue
from acab.core.value.sentence import Sentence
from acab.core.containers import action
from acab.core.containers import transform
from acab.core.value.instruction import ProductionComponent, ProductionContainer

from acab.modules.values import numbers
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.modules.values.numbers.util import FLOAT_t, INT_t
from acab.modules.parsing.exlo ActionParser as AP
from acab.modules.parsing.exlo FactParser as FP
from acab.modules.parsing.exlo TransformParser as TP
from acab.working_memory.trie_wm.trie_working_memory import TrieWM
from acab.core.semantics.print_semantics import AcabPrintSemantics
from acab.core.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR,
              ProductionComponent: ([DH.component_substruct], DH.component_sentinel),
              # ProductionContainer: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel)}
              }

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})


class NumberParseTests(unittest.TestCase):
    ns = None

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

        # setup class
        NumberParseTests.ns = numbers.MODULE()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        self.trie = TrieWM()
        self.trie.construct_parsers_from_fragments([NumberParseTests.ns])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_int_number_parsing(self):
        result = FP.parse_string("number.test.20")[0]
        self.assertIsNotNone(result)
        self.assertEqual(result[-1].type, INT_t)
        self.assertEqual(result[-1].value, 20)

    def test_float_number_parsing(self):
        result = FP.parse_string("number.test.20d325")[0]
        self.assertIsNotNone(result)
        self.assertEqual(result[-1].type, FLOAT_t)
        self.assertEqual(result[-1].value, 20.325)


    def test_simple_transform_parse(self):
        result = TP.parse_string("λoperator.transform.add 20 30 -> $z")
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.transform.add')
        self.assertEqual([x.value for x in result.clauses[0].params], [20, 30])


    def test_transform_parse(self):
        result = TP.parse_string('λoperator.transform.add 2 3 -> $z, λoperator.transform.sub 3 2 -> $a, λoperator.transform.mul 2 2 -> $b')
        self.assertEqual(len(result), 3)
        self.assertTrue(all([isinstance(x, transform.TransformComponent) for x in result.clauses]))
        for parsed_action, op in zip(result, ['add','sub', 'mul']):
            self.assertEqual(Printer.print(parsed_action.op), "operator.transform.{}".format(op))


    def test_transform_str_equal(self):
        logmod.getLogger().setLevel(0)
        actions = ["λoperator.transform.add 2 4 -> $x", "λoperator.transform.sub 3 5 -> $y", "λoperator.transform.round 4 -> $z"]
        parsed = [TP.parse_string(x) for x in actions]
        zipped = zip(actions, parsed)
        breakpoint()

        logging.info(Printer.print(parsed[0]))
        # for x,y in zipped:
        #     self.assertEqual(x, Printer.print(y).strip())


    def test_numbers_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = round(random.random() * 1000)
            result = FP.parse_string('a.' + str(r))[0]
            self.assertEqual(result[-1].value, r)


    def test_negative_number_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = - round(random.random() * mult)
            result = FP.parse_string('a.'+str(r))[0]
            self.assertEqual(result[-1].value, r)


    def test_floats(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            a = round(random.random() * mult)
            b = round(random.random() * mult)
            float_form = float(str(a) + "." + str(b))
            d_form = str(a) + "d" + str(b)
            result = FP.parse_string('a.'+d_form)[0]
            self.assertEqual(result[-1].value, float_form)
