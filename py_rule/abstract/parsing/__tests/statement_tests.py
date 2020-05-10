#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
import pyparsing as pp
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.sentence import Sentence

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
        basic_node_parser.setParseAction(lambda toks: Sentence([PyRuleNode(toks[0])]))

        basic_value_parser = pp.Keyword('value') + pp.lineEnd
        basic_value_parser.setParseAction(lambda toks: ('value', PyRuleValue(toks[0])))

        statement_p = PU.STATEMENT_CONSTRUCTOR(pp.Keyword('blah'),
                                               basic_node_parser,
                                               basic_value_parser)

        result = statement_p.parseString("test: (::blah)\n#test\n\nvalue\nend")

        value = result[0][-1]._value

        self.assertTrue('test' in value._tags)

    def test_basic_tag_plural(self):
        basic_node_parser = pp.Keyword('test')
        basic_node_parser.setParseAction(lambda toks: Sentence([PyRuleNode(toks[0])]))

        basic_value_parser = pp.Keyword('value') + pp.lineEnd
        basic_value_parser.setParseAction(lambda toks: ('value', PyRuleValue(toks[0])))

        statement_p = PU.STATEMENT_CONSTRUCTOR(pp.Keyword('blah'),
                                               basic_node_parser,
                                               basic_value_parser)

        result = statement_p.parseString("test: (::blah)\n#abcd, #aaaa, #bbbb\n\nvalue\nend")
        value = result[0][-1]._value

        self.assertTrue('abcd' in value._tags)
        self.assertTrue('aaaa' in value._tags)
        self.assertTrue('bbbb' in value._tags)




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
