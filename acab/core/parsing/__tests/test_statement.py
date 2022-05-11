#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import warnings

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()

from acab.core.data.node import AcabNode
from acab.core.parsing import parsers as PU
from acab.core.parsing.statement_core import StatementCore
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue


class BasicStatement(Instruction):

    def __contains__(self, val):
        return False

    def __len__(self):
        return 0

    def __hash__(self):
        return hash(self.value)


class StatementTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_basic_statement(self):
        """ Check a constructed statement can have tags """
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.set_parse_action(lambda s, l, toks: Sentence([AcabValue(toks[0])]))

        basic_value_parser = pp.Empty()
        basic_value_parser.set_parse_action(lambda s, l, toks: BasicStatement(["blah"]))

        statement_p = StatementCore(basic_node_parser,
                                    basic_value_parser)

        result = statement_p.parse_string("a_statement(::test):\n\nend")[0]
        self.assertIsInstance(result, BasicStatement)
        self.assertEqual(result.name, "a_statement")


    def test_basic_tag(self):
        """ Check a constructed statement can have tags """
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.set_parse_action(lambda s, l, toks: Sentence([AcabValue(toks[0])]))

        basic_value_parser = pp.Keyword('value')
        basic_value_parser.set_parse_action(lambda s, l, toks: BasicStatement(toks[0]))

        statement_p = StatementCore(basic_node_parser,
                                    basic_value_parser)

        result = statement_p.parse_string("a_statement(::test):\n #test.blah\n\nvalue\nend")[0]

        tags_str = [x for x in result.tags]
        self.assertTrue('test' in tags_str)

    def test_basic_tag_plural(self):
        """ Check a constructed statement can have multiple tags """
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.set_parse_action(lambda s, l, toks: Sentence([AcabValue(toks[0])]))

        basic_value_parser = pp.Keyword('value')
        basic_value_parser.set_parse_action(lambda s, l, toks: BasicStatement(toks[0]))

        statement_p = StatementCore(basic_node_parser,
                                    basic_value_parser)

        result = statement_p.parse_string("a_statement(::test):\n#abcd\n#aaaa\n#bbbb\n\nvalue\nend")[0]
        value = result

        tags_str = [x for x in value.tags]

        self.assertTrue('abcd' in tags_str)
        self.assertTrue('aaaa' in tags_str)
        self.assertTrue('bbbb' in tags_str)
