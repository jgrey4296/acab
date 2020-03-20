#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.working_memory.trie_wm.parsing import RuleParser as RP

class NumberTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    @unittest.skip("Example")
    def test_bdi_rule_parse(self):
        rulestr = """
bdi.blah:
    #propose
    count!$x(< 10)?

    $x + 2 -> $y
    ~{} "Hello: {x}" -> $z

    @ ($z)
    + (count!$y)
end
        """.strip()
        result = RP.parseString(rulestr)[0]
        self.assertEqual(result[0], KBU.RULE_S)
        self.assertIsInstance(result[1], Rule)



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
