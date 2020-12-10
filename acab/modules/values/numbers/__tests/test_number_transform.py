#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import pyparsing as pp
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import TransformParser as TP
from acab.working_memory.trie_wm.parsing import FactParser as FP
from acab.working_memory.trie_wm.parsing import QueryParser as QP

from acab.abstract.rule.production_abstractions import ProductionComponent, ProductionOperator
from acab.modules.values import numbers
from acab.working_memory.trie_wm.trie_working_memory import TrieWM
from acab.working_memory.trie_wm import util as KBU
from acab.abstract.printing.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})


class NumberTransformTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

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
        result = TP.transform_core.parseString(r'λoperator.transform.add $x 20 -> $y')[0]
        self.assertIsInstance(result, transform.TransformComponent)
        self.assertEqual(Printer.print(result.op), "operator.transform.add")
        self.assertEqual(len(result._params), 2)



    def test_basic_transform_core_rebind(self):
        result = TP.transform_core.parseString('λoperator.transform.mul $y 20 -> $z')[0]
        self.assertIsInstance(result, transform.TransformComponent)
        self.assertEqual(Printer.print(result.op), "operator.transform.mul")
        self.assertEqual(result._params[0]._value, "y")
        self.assertTrue(result._params[0].is_var)
        self.assertEqual(result._params[1]._value, 20)
        self.assertIsNotNone(result._rebind)
        self.assertEqual(result._rebind._value, 'z')


    def test_basic_transform(self):
        result = TP.parseString('λoperator.transform.add $x 20 -> $y, λoperator.transform.add $y 5 -> $z')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 2)


    def test_binary_operator(self):
        result = TP.parseString('λoperator.transform.add $x 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.add")
        self.assertEqual(result.clauses[0]._params[0]._value, 'x')
        self.assertEqual(result.clauses[0]._params[1]._value, 20)
        self.assertIsNotNone(result.clauses[0]._rebind)


    def test_binary_rebind(self):
        result = TP.parseString('λoperator.transform.add $x 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.add")
        self.assertEqual(result.clauses[0]._params[0]._value, 'x')
        self.assertEqual(result.clauses[0]._params[1]._value, 20)
        self.assertEqual(result.clauses[0]._rebind._value, 'y')

    def test_unary_round(self):
        result = TP.parseString('λoperator.transform.round $x -> $y')
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.transform.round')

    def test_binary_rand_operator(self):
        result = TP.parseString('λoperator.transform.rand $x $y -> $z')
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.transform.rand')

    def test_unary_operator(self):
        result = TP.parseString(r'λoperator.transform.neg $x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.neg")
        self.assertEqual(result.clauses[0]._params[0]._value, "x")
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_unary_rebind(self):
        result = TP.parseString(r'λoperator.transform.neg $x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), "operator.transform.neg")
        self.assertEqual(result.clauses[0]._params[0]._value, "x")
        self.assertIsNotNone(result.clauses[0]._rebind)
        self.assertEqual(result.clauses[0]._rebind._value, 'y')



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
            parsed = TP.parseString(a_string)
            self.assertEqual(Printer.print(parsed).strip(), a_string)


