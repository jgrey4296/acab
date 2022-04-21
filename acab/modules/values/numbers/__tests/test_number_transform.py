#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import pyparsing as pp
import unittest
import logging as logmod
logging = logmod.getLogger(__name__)

import acab
acab.setup()

from acab.core.value.value import AcabValue
from acab.core.value.sentence import Sentence
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.modules.parsing.exlo import ActionParser as AP
from acab.modules.parsing.exlo import TransformParser as TP
from acab.modules.parsing.exlo import FactParser as FP
from acab.modules.parsing.exlo import QueryParser as QP

from acab.core.value.instruction import ProductionComponent, ProductionOperator
from acab.modules.values import numbers
from acab.core.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})


class NumberTransformTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

        NumberTransformTests.ns = numbers.MODULE()

    def setUp(self):
        self.trie = TrieWM()
        self.trie.construct_parsers_from_fragments([NumberTransformTests.ns])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass

    def test_basic_transform_core(self):
        breakpoint()
        result = TP.transform_core.parse_string(r'λoperator.transform.add $x 20 -> $y')[0]
        self.assertIsInstance(result, transform.TransformComponent)
        self.assertEqual(Printer.print(result.op), "operator.transform.add")
        self.assertEqual(len(result.params), 2)



    def test_basic_transform_core_rebind(self):
        result = TP.transform_core.parse_string('λoperator.transform.mul $y 20 -> $z')[0]
        self.assertIsInstance(result, transform.TransformComponent)
        self.assertEqual(Printer.print(result.op), "operator.transform.mul")
        self.assertEqual(result.params[0].value, "y")
        self.assertTrue(result.params[0].is_var)
        self.assertEqual(result.params[1].value, 20)
        self.assertIsNotNone(result._rebind)
        self.assertEqual(result._rebind.value, 'z')


    def test_basic_transform(self):
        result = TP.parse_string('λoperator.transform.add $x 20 -> $y, λoperator.transform.add $y 5 -> $z')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 2)


    def test_binary_operator(self):
        result = TP.parse_string('λoperator.transform.add $x 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.add")
        self.assertEqual(result.clauses[0].params[0].value, 'x')
        self.assertEqual(result.clauses[0].params[1].value, 20)
        self.assertIsNotNone(result.clauses[0]._rebind)


    def test_binary_rebind(self):
        result = TP.parse_string('λoperator.transform.add $x 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.add")
        self.assertEqual(result.clauses[0].params[0].value, 'x')
        self.assertEqual(result.clauses[0].params[1].value, 20)
        self.assertEqual(result.clauses[0]._rebind.value, 'y')

    def test_unary_round(self):
        result = TP.parse_string('λoperator.transform.round $x -> $y')
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.transform.round')

    def test_binary_rand_operator(self):
        result = TP.parse_string('λoperator.transform.rand $x $y -> $z')
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.transform.rand')

    def test_unary_operator(self):
        result = TP.parse_string(r'λoperator.transform.neg $x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.neg")
        self.assertEqual(result.clauses[0].params[0].value, "x")
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_unary_rebind(self):
        result = TP.parse_string(r'λoperator.transform.neg $x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.neg")
        self.assertEqual(result.clauses[0].params[0].value, "x")
        self.assertIsNotNone(result.clauses[0]._rebind)
        self.assertEqual(result.clauses[0]._rebind.value, 'y')



    def test_fact_str_equal(self):
        transforms = ["λoperator.transform.add $x 20 -> $y",
                      "λoperator.transform.add $x 20 -> $y\nλoperator.transform.add $y 5 -> $z",
                      "λoperator.transform.sub $x 10 -> $y",
                      "λoperator.transform.mul $x 100 -> $y",
                      "λoperator.transform.add $x 20 -> $y",
                      "λoperator.transform.add $blah $bloo -> $BLEE",
                      "λoperator.transform.neg $x -> $y",
                      "λoperator.transform.round $x -> $y",
                      "λoperator.transform.neg $x -> $y",
                      "λoperator.transform.round $x -> $y",
                      "λoperator.transform.regex $x /blah/ $a -> $z",
                      "λoperator.transform.regex $x /aw/ $b -> $blah",
                      "λoperator.transform.add $x 2d5 -> $y"
        ]
        for a_string in transforms:
            parsed = TP.parse_string(a_string)
            self.assertEqual(Printer.print(parsed).strip(), a_string)


