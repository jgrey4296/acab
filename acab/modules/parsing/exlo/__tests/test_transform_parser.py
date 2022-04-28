import unittest
from os.path import splitext, split
import logging as logmod
logging = logmod.getLogger(__name__)

import re
import pyparsing as pp
import acab
config = acab.setup()

if '@pytest_ar' in globals():
    from acab.core.parsing import debug_funcs as DBF
    DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

from acab.core.value import default_structure as DS
from acab.core.value.value import AcabValue
from acab.core.value.sentence import Sentence
from acab.core.value.instruction import ProductionOperator, ProductionContainer, ProductionComponent
from acab.core.parsing import parsers as PU

from acab.modules.parsing.exlo.parsers import TransformParser as TP
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

        TP.HOTLOAD_TRANS_OP << PU.OPERATOR_SUGAR

    @classmethod
    def tearDownClass(cls):
        TP.HOTLOAD_TRANS_OP << pp.Empty()
    #----------
    def test_transform_core(self):
        """ Check a transform can be parsed """
        result = TP.transform_core.parse_string("λa.b.c $x $y -> $z")[0]
        self.assertIsInstance(result, ProductionComponent)
        self.assertEqual(result.rebind.name, "z")
        self.assertIn(DS.OPERATOR, result.op.data)


    def test_transforms(self):
        """ Check multiple transforms can be parsed """
        result = TP.transforms.parse_string("  λa.b.c $x -> $y\n  λa.b.d $x -> $z")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 2)
        self.assertIn(DS.OPERATOR, result.clauses[0].op.data)
        self.assertIn(DS.OPERATOR, result.clauses[1].op.data)

    def test_non_path_operator(self):
        result = TP.transform_sugar.parse_string("$x ∈ $y -> $z")[0]
        self.assertIsInstance(result, ProductionComponent)
        self.assertEqual(result.rebind.name, "z")
        self.assertIn(DS.OPERATOR, result.op.data)
