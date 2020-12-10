import unittest
# Setup root_logger:
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)
##############################

from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.core.values import Sentence
from acab.abstract.rule.production_abstractions import ProductionOperator, ProductionComponent, ProductionContainer
from acab.abstract.printing.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR,
              ProductionComponent: ([DH.component_substruct], DH.component_sentinel)
              }

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

def S(*words):
    return Sentence.build(words)

class Trie_Action_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)

        logging = root_logger.getLogger(__name__)
        logging.root.addHandler(console)
        logging.root.addHandler(file_h)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_string_value(self):
        result = AP.parseString('λS.ActionAdd "blah bloo" "blee" "awef"')
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(Printer.print(result.clauses[0].op), "S.ActionAdd")
        self.assertEqual([x._value for x in result.clauses[0]._params], ["blah bloo","blee","awef"])

    def test_actions_fact_str(self):
        result = AP.parseString('λS.ActionAdd a.b.c, λoperator.add ~a!b.d, λoperator.add $x, λoperator.add $x.a.b')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result), 4)
        self.assertTrue(all([isinstance(x, ProductionComponent) for x in result]))
        self.assertEqual(Printer.print(result.clauses[0]._params[0]), "a.b.c")
        self.assertEqual(Printer.print(result.clauses[1]._params[0]), "~a!b.d")
        self.assertEqual(Printer.print(result.clauses[2]._params[0]), "$x.")
        self.assertEqual(Printer.print(result.clauses[3]._params[0]), "$x.a.b")

    def test_action_binding_expansion(self):
        logging.info("TESTING")
        bindings = {"x" : FP.parseString('a.b.c')[0] }
        parsed_action = AP.parseString("λoperator.add $x")
        bound_action = parsed_action.bind(bindings)
        self.assertIsInstance(bound_action, ProductionContainer)

        result = Printer.print(bound_action.clauses[0])
        self.assertEqual(result, r"λoperator.add (a.b.c)")

    def test_action_definition(self):
        test_str = "test: (::α)\nλoperator.add a.b.c\n\nend"
        definition = AP.action_definition.parseString(test_str)
        self.assertEqual(definition[0][-1].name, "test")

    def test_parse_action_no_params(self):
        test_str = "λoperator.add"

        result = AP.action_component.parseString(test_str)[0]
        self.assertIsInstance(result, ProductionComponent)

    # TODO test  sugar, test sentences, test values, test multi params
    @unittest.skip("TODO")
    def test_action_sugar(self):
        return


