#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
import pyparsing as pp

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.parsing import util as PU
from acab.abstract.node import AcabNode
from acab.abstract.value import AcabValue, AcabStatement
from acab.abstract.sentence import Sentence

class StatementTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_basic_tag(self):
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.setParseAction(lambda toks: Sentence([AcabValue(toks[0])]))

        basic_value_parser = pp.Keyword('value') + pp.lineEnd
        basic_value_parser.setParseAction(lambda toks: ('value', AcabStatement(toks[0])))

        statement_p = PU.STATEMENT_CONSTRUCTOR(pp.Keyword('blah'),
                                               basic_node_parser,
                                               basic_value_parser)

        result = statement_p.parseString("test: (::blah)\n#test\n\nvalue\nend")[0]
        tags_str = [x for x in result[-1]._tags]
        self.assertTrue('test' in tags_str)

    def test_basic_tag_plural(self):
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.setParseAction(lambda toks: Sentence([AcabValue(toks[0])]))

        basic_value_parser = pp.Keyword('value') + pp.lineEnd
        basic_value_parser.setParseAction(lambda toks: ('value', AcabStatement(toks[0])))

        statement_p = PU.STATEMENT_CONSTRUCTOR(pp.Keyword('blah'),
                                               basic_node_parser,
                                               basic_value_parser)

        result = statement_p.parseString("test: (::blah)\n#abcd, #aaaa, #bbbb\n\nvalue\nend")[0]
        value = result[-1]

        tags_str = [x for x in value._tags]

        self.assertTrue('abcd' in tags_str)
        self.assertTrue('aaaa' in tags_str)
        self.assertTrue('bbbb' in tags_str)




if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
