import logging as root_logger
logging = root_logger.getLogger(__name__)

import random
import unittest

from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

import acab.modules.analysis.typing.parsing.TypeDefParser as TD
import acab.modules.analysis.typing.parsing.TypeParser as TP

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.parsing import parsers as PU

from acab.error.acab_parse_exception import AcabParseException

from acab.modules.analysis.typing import util as TU
from acab.modules.analysis.typing.values.operator_definition import OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import TypeDefinition
from acab.working_memory.trie_wm.parsing import FactParser as FP
from acab.abstract.printing.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

class TypeDef_ParserTests(unittest.TestCase):

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
        FP.HOTLOAD_ANNOTATIONS << TP.TYPEDEC_CORE
        TP.HOTLOAD_BASIC_SEN << FP.BASIC_SEN
        TD.HOTLOAD_BASIC_SEN << FP.BASIC_SEN
        TD.HOTLOAD_PARAM_SEN << FP.PARAM_SEN

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_trivial(self):
        self.assertIsNotNone(TD.parseString)
        self.assertIsNotNone(TD.RECORD_TYPE)
        self.assertIsNotNone(TD.SUM_TYPE)
        self.assertIsNotNone(TD.TYPE_CLASS_DEF)
        self.assertIsNotNone(TD.OP_DEF)

    def test_basic_non_structure_typedef(self):
        result = TD.parseString("blah.x: (::σ) end")[0]
        self.assertIsInstance(result[-1], TypeDefinition)

    def test_basic_typedef(self):
        result = TD.parseString("blah.x: (::σ)\na.b.c\n\nend")[0]
        self.assertIsInstance(result[-1], TypeDefinition)
        self.assertEqual(result[-1].is_var, False)
        self.assertEqual(result[-1].type, TU.TYPE_DEFINITION)
        self.assertEqual(len(result[-1]._params), 0)
        self.assertEqual(result[-1].name, "x")
        self.assertEqual(len(result[-1].structure), 1)

    def test_typedef_with_variable(self):
        result = TD.parseString('blah.structure: (::σ)\n |$x |\n\na.b.$x\n\nend')[0]
        self.assertIsInstance(result[-1], TypeDefinition)
        self.assertEqual(len(result[-1]._params), 1)
        self.assertEqual(result[-1]._params[0], "x")

    def test_typedef_with_multi_variables(self):
        result = TD.parseString('blah.x: (::σ)\n | $x, $y |\n\na.b.$x, a.b.$y\n\nend')[0]
        self.assertIsInstance(result[-1], TypeDefinition)
        self.assertEqual(len(result[-1]._params), 2)
        var_set = set([x.value for x in result[-1]._params])
        match_set = set(["x", "y"])
        self.assertEqual(var_set, match_set)

    def test_typedef_with_structure_types(self):
        result = TD.parseString('blah.x: (::σ)\na.b.c(::bloo)\n\nend')[0]
        self.assertEqual(Printer.print(result[-1].structure[0][-1].type), '::bloo')

    @unittest.skip("TODO: broken, needs to fix typedef var checks")
    def test_typedef_with_bad_vars(self):
        with self.assertRaises(AcabParseException):
            result = TD.parseString('blah.x: (::σ)\n| $x |\n\na.b.c\n\nend')[0]

    def test_op_def_parse(self):
        the_string = 'AddOp: (::λ) $x(::Num).$x.$x => +'
        result = TD.OP_DEF.parseString(the_string)[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[-1], OperatorDefinition)

    def test_op_def_parse_no_sugar(self):
        the_string = 'AddOp: (::λ) $x(::Num).$x.$x'
        result = TD.OP_DEF.parseString(the_string)[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[-1], OperatorDefinition)


