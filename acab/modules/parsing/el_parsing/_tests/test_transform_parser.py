import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

import re

import acab
acab.setup()

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.parsing.bootstrap_parser import BootstrapParser
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionContainer
from acab.working_memory.trie_wm import util as KBU
from acab.modules.parsing.el_parsing TransformParser as TP

from acab.modules.parsing.el_parsing ActionParser as AP
from acab.modules.parsing.el_parsing FactParser as FP

from acab.abstract.semantics.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR,
              }

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        # setup class
        bp = BootstrapParser()
        TP.HOTLOAD_TRANS_OP << bp.query('operator.ProductionContainer.*',
                                        'operator.sugar')
        TP.HOTLOAD_TRANS_STATEMENTS << bp.query("operator.ProductionContainer.statements.*",
                                                "operator.sugar")

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_ternary_operator(self):
        result = TP.parseString('λoperator.ProductionContainer.regex $x /blah/ $a -> $y')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.ProductionContainer.regex')
        self.assertEqual(result.clauses[0].params[0].value, 'x')
        self.assertEqual(result.clauses[0].params[1].value, re.compile('blah'))
        self.assertEqual(result.clauses[0].params[2].value, 'a')
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_ternary_operator_rebind(self):
        result = TP.parseString('λoperator.ProductionContainer.regex $x /blah/ $awef -> $q')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.ProductionContainer.regex')
        self.assertEqual(result.clauses[0].params[0].name, 'x')
        self.assertEqual(result.clauses[0].params[1].value, re.compile('blah'))
        self.assertEqual(result.clauses[0].params[2].name, 'awef')
        self.assertEqual(result.clauses[0]._rebind.name, 'q')

    def test_unary_format(self):
        result = TP.parseString('λoperator.ProductionContainer.format $x blah -> $y')
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.ProductionContainer.format')


    # TODO test ProductionContainer sugar, test multi variables, test sentences, test values
    @unittest.skip("TODO")
    def test_transform_sugar(self):
        return

