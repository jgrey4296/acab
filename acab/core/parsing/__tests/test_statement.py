#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as logmod
logging = logmod.getLogger(__name__)

import pyparsing as pp

import acab
acab.setup()

from acab.core.parsing import parsers as PU
from acab.core.data.value import AcabValue
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.data.node import AcabNode

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
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_basic_tag(self):
        """ Check a constructed statement can have tags """
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.set_parse_action(lambda s, l, toks: Sentence([AcabValue(toks[0])]))

        basic_value_parser = pp.Keyword('value')
        basic_value_parser.set_parse_action(lambda s, l, toks: BasicStatement(toks[0]))

        statement_p = PU.STATEMENT_CONSTRUCTOR(basic_node_parser,
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

        statement_p = PU.STATEMENT_CONSTRUCTOR(basic_node_parser,
                                               basic_value_parser)

        result = statement_p.parse_string("a_statement(::test):\n#abcd\n#aaaa\n#bbbb\n\nvalue\nend")[0]
        value = result

        tags_str = [x for x in value.tags]

        self.assertTrue('abcd' in tags_str)
        self.assertTrue('aaaa' in tags_str)
        self.assertTrue('bbbb' in tags_str)
