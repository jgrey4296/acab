import unittest
import logging
from test_context import py_rule
from py_rule.util import VALUE_TYPE_S
from py_rule.modules.time.parsing import parser as tp
from py_rule.knowledge_bases.trie_kb.parsing import FactParser as fp
from py_rule.abstract.sentence import Sentence
from py_rule.modules.time.pattern import Pattern

class TrieIntegrationTimeTests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_basic_parser_extension(self):
        fp.OTHER_VALS << tp.main_pattern

        a = fp.parseString("a.test.sentence.[[a b c $d]]")[0]
        self.assertIsInstance(a, Sentence)
        self.assertIsInstance(a[-1]._value, Pattern)
        self.assertEqual(a[-1]._data[VALUE_TYPE_S], "pattern")

if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.TrieIntegrationTimeTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
