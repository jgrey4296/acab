import logging as logmod
import unittest

import acab

config = acab.setup()

import acab.modules.parsing.exlo.FactParser as FP
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.core.printing import default_handlers as DH
from acab.interfaces.semantic import PrintSemantics_i

NEGATION_S      = config.prepare("Parse.Structure", "NEGATION")()
TYPE_INSTANCE_S = config.prepare("Parse.Structure", "TYPE_INSTANCE")()

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR,
              ProductionComponent: ([DH.component_substruct], DH.component_sentinel)
              }

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

logging = logmod.getLogger(__name__)

class TestPrintSemantics(unittest.TestCase):

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_parse_string(self):
        result = FP.parse_string('a.b.c')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(all([isinstance(x, AcabValue) for x in result]))

        self.assertEqual(Printer.print(result), "a.b.c")
        self.assertTrue(all([x.data['exop'] == config.modal_enums['exop'].DOT for x in result]))

    def test_fact_str_equal(self):
        actions = ["a.b.c",
                   "a.b!c",
                   'a.b."a string".c',
                   'a.b!"a string"!c',
                   'a.b.$x',
                   'a!$x!y']
        parsed = [FP.parse_string(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for act,par in zipped:
            self.assertEqual(act,Printer.print(par))

    def test_leading_bind_str_equal(self):
        actions = ['$x.a.b.c', '$y!b.c', '$x.$y!$z']
        parsed = [FP.parse_string(x)[0] for x in actions]
        zipped = zip(actions, parsed)

        for a,p in zipped:
            self.assertEqual(a, Printer.print(p))

    def test_binding_expansion(self):
        bindings = { "a" : FP.parse_string("blah")[0],
                     "b": FP.parse_string("bloo")[0] }
        result = FP.parse_string('$a.b.$b!c')[0]
        expanded = result.bind(bindings)
        asString = Printer.print(expanded)
        self.assertEqual(asString, "blah.b.bloo!c")

    def test_nested_sentence_statement(self):
        result = FP.SEN_STATEMENT.parse_string("a.test.sentence: (::Σ)\ninternal.nested: (::Σ)\ninternal.one\ninternal.two\nend\nblah.bloo.blee\nend")
        self.assertEqual(len(result), 3)
        self.assertEqual(Printer.print(result[0]), "a.test.sentence.internal.nested.internal.one")
        self.assertEqual(Printer.print(result[1]), "a.test.sentence.internal.nested.internal.two")
        self.assertEqual(Printer.print(result[2]), "a.test.sentence.blah.bloo.blee")

    def test_basic_regex_comparison(self):
        result = FP.QUERY_OP_Internal.parse_string(r'λoperator.query.regmatch /blah/')[0]
        self.assertIsInstance(result, tuple)
        qc = result[1]
        self.assertIsInstance(qc, ProductionComponent)
        self.assertEqual(Printer.print(qc.op), 'operator.query.regmatch')
        # Note the end modal '.'
        self.assertEqual(Printer.print(qc.params[0]), '/blah/.')
        # TODO convert this to a type system lookup
        self.assertEqual(qc.params[0].type, Sentence.build([REGEX_PRIM]))

    def test_comparison_parse(self):
        result = FP.PARAM_BINDING_END.parse_string("testing(λoperator.query.regmatch /test/)")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0].data[CONSTRAINT_V][0].op), "operator.query.regmatch")

    def test_comparison_parse_2(self):
        result = FP.PARAM_BINDING_CORE.parse_string("testing(λoperator.query.regmatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0].data[CONSTRAINT_V][0].op), "operator.query.regmatch")

    def test_comparison_parse_variable(self):
        result = FP.PARAM_BINDING_CORE.parse_string("$x(λoperator.query.regmatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0].data[CONSTRAINT_V][0].op), "operator.query.regmatch")


    def test_comparison_in_clause(self):
        result = QP.clause.parse_string("a.testing(λoperator.query.regmatch /test/).clause?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0][1].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "operator.query.regmatch")


    def test_tag_query(self):
        result = QP.clause.parse_string("a.testing(#testTag)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0][1].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")

    def test_tag_list_query(self):
        result = QP.clause.parse_string("a.testing(#first, #second, #third)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0][1].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")

        tags = [x.name for x in result[0][1].data[CONSTRAINT_V][0].params]
        self.assertEqual(tags, ["first", "second", "third"])

    def test_tag_list_interaction(self):
        result = QP.clause.parse_string("a.testing(#first, #second, λoperator.query.regmatch /Test/)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 2)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][1].op), "operator.query.regmatch")

    def test_tag_list_interaction_2(self):
        result = QP.clause.parse_string("a.testing(λoperator.query.regmatch /Test/, #test, #second)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 2)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][1].op), "HasTag")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "operator.query.regmatch")

    def test_tag_list_interaction_3(self):
        result = QP.clause.parse_string("a.testing(#aTag, λoperator.query.regmatch /Test/, #test, #second)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 3)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][1].op), "operator.query.regmatch")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][2].op), "HasTag")

    def test_ternary_operator(self):
        result = TP.parse_string('λoperator.ProductionContainer.regex $x /blah/ $a -> $y')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.ProductionContainer.regex')
        self.assertEqual(result.clauses[0].params[0].value, 'x')
        self.assertEqual(result.clauses[0].params[1].value, re.compile('blah'))
        self.assertEqual(result.clauses[0].params[2].value, 'a')
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_ternary_operator_rebind(self):
        result = TP.parse_string('λoperator.ProductionContainer.regex $x /blah/ $awef -> $q')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.ProductionContainer.regex')
        self.assertEqual(result.clauses[0].params[0].name, 'x')
        self.assertEqual(result.clauses[0].params[1].value, re.compile('blah'))
        self.assertEqual(result.clauses[0].params[2].name, 'awef')
        self.assertEqual(result.clauses[0]._rebind.name, 'q')

    def test_unary_format(self):
        result = TP.parse_string('λoperator.ProductionContainer.format $x blah -> $y')
        self.assertEqual(Printer.print(result.clauses[0].op), 'operator.ProductionContainer.format')

    def test_string_value(self):
        result = AP.parse_string('λS.ActionAdd "blah bloo" "blee" "awef"')
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(Printer.print(result.clauses[0].op), "S.ActionAdd")
        self.assertEqual([x.value for x in result.clauses[0].params], ["blah bloo","blee","awef"])

    def test_actions_fact_str(self):
        result = AP.parse_string('λS.ActionAdd a.b.c, λoperator.add ~a!b.d, λoperator.add $x, λoperator.add $x.a.b')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result), 4)
        self.assertTrue(all([isinstance(x, ProductionComponent) for x in result]))
        self.assertEqual(Printer.print(result.clauses[0].params[0]), "a.b.c")
        self.assertEqual(Printer.print(result.clauses[1].params[0]), "~a!b.d")
        self.assertEqual(Printer.print(result.clauses[2].params[0]), "$x.")
        self.assertEqual(Printer.print(result.clauses[3].params[0]), "$x.a.b")

    def test_action_binding_expansion(self):
        logging.info("TESTING")
        bindings = {"x" : FP.parse_string('a.b.c')[0] }
        parsed_action = AP.parse_string("λoperator.add $x")
        bound_action = parsed_action.bind(bindings)
        self.assertIsInstance(bound_action, ProductionContainer)

        result = Printer.print(bound_action.clauses[0])
        self.assertEqual(result, r"λoperator.add (a.b.c)")

    def test_name_empty_rule_print(self):
        result = RP.parse_string("a.rule.x: (::ρ) end")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertEqual(Printer.print(result[0][-1]).strip(), "x: (::ρ) end")

    def test_rule_simple_binding_expansion(self):
        bindings = { "x" : FP.parse_string('a.b.c')[0] }
        result = RP.parse_string("a.rule.x: (::ρ)\n\n$x?\n\nend")[0]
        expanded = result[-1].bind(bindings)
        self.assertEqual(Printer.print(expanded).strip(),
                         "AnonRule: (::ρ)\n    a.b.c?\nend")

    def test_rule_tags(self):
        the_str = 'a.test.rule.x: (::ρ)\n    #blah, #blee, #bloo\n\n    a.b.c?\n\n    λoperator.action.add a.b.c\nend'
        result = RP.parse_string(the_str)[0]
        self.assertIsInstance(result[-1], ProductionStructure)
        self.assertEqual(Printer.print(result).strip(), the_str)
        tags = [x for x in result[-1]._tags]
        self.assertTrue(all(x in tags for x in ["blah","bloo","blee"]))
