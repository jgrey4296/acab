#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
import random

from acab import util
from acab.abstract import action
from acab.abstract import transform
from acab.abstract.printing import util as PrU
from acab.modules.values import numbers
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.modules.values.numbers.util import FLOAT, INT
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP
from acab.working_memory.trie_wm.parsing import TransformParser as TP
from acab.working_memory.trie_wm.trie_working_memory import TrieWM


class NumberParseTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        NumberParseTests.ns = numbers.MODULE()

    def setUp(self):
        self.trie = TrieWM()
        self.trie.add_modules(["acab.modules.operators.standard_operators", NumberParseTests.ns])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_int_number_parsing(self):
        result = FP.parseString("number.test.20")[0]
        self.assertIsNotNone(result)
        self.assertEqual(result[-1].type, INT)
        self.assertEqual(result[-1]._value, 20)

    def test_float_number_parsing(self):
        result = FP.parseString("number.test.20d325")[0]
        self.assertIsNotNone(result)
        self.assertEqual(result[-1].type, FLOAT)
        self.assertEqual(result[-1]._value, 20.325)


    def test_simple_transform_parse(self):
        result = TP.parseString("20 \operator.transform.n_ary.add 30 -> $z")
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(result.clauses[0].op, 'operator.transform.n_ary.add')
        self.assertEqual([x._value for x in result.clauses[0]._params], [20, 30])


    def test_transform_parse(self):
        result = TP.parseString('2 \operator.transform.n_ary.add 3 -> $z, 3 \operator.transform.n_ary.sub 2 -> $a, 2 \operator.transform.n_ary.mul 2 -> $b')
        self.assertEqual(len(result), 3)
        self.assertTrue(all([isinstance(x, transform.TransformComponent) for x in result.clauses]))
        for parsed_action, op in zip(result, ['add','sub', 'mul']):
            self.assertEqual(parsed_action.op, "operator.transform.n_ary.{}".format(op))


    def test_transform_str_equal(self):
        actions = ["2 \operator.transform.n_ary.add 4 -> $x", "3 \operator.transform.n_ary.sub 5 -> $y", "\operator.transform.n_ary.round 4 -> $z"]
        parsed = [TP.parseString(x) for x in actions]
        zipped = zip(actions, parsed)
        for x,y in zipped:
            self.assertEqual(x, y.pprint().strip())


    def test_numbers_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = round(random.random() * 1000)
            result = FP.parseString('a.' + str(r))[0]
            self.assertEqual(result[-1]._value, r)


    def test_negative_number_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = - round(random.random() * mult)
            result = FP.parseString('a.'+str(r))[0]
            self.assertEqual(result[-1]._value, r)


    def test_floats(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            a = round(random.random() * mult)
            b = round(random.random() * mult)
            float_form = float(str(a) + "." + str(b))
            d_form = str(a) + "d" + str(b)
            result = FP.parseString('a.'+d_form)[0]
            self.assertEqual(result[-1]._value, float_form)



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
