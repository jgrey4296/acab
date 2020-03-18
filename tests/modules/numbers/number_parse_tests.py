#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from test_context import py_rule
from py_rule.modules.values.numbers.parsers import NumberParser as AP

class ActionBlah(action.ActionOp):
    def __init__(self):
        super().__init__()

    def __call__(self, engine, params):
        logging.info("Blah")


class NumberTests(unittest.TestCase):

    @classmethod
    def setUpClas(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_simple_action_parse(self):
        result = AP.parseString("+(20, 30, 40)")[0]
        self.assertIsInstance(result, action.Action)
        self.assertEqual(result._op._op_str, '+')
        self.assertEqual([x[-1]._value for x in result._values], [20, 30, 40])


    def test_custom_action_parse(self):
        result = AP.parseString("blah(20, 30, 40)")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], action.Action)
        self.assertEqual(result[0]._op._op_str, "blah")
        self.assertEqual([x[-1]._value for x in result[0]._values], [20, 30, 40])


    def test_actions_parse(self):
        result = AP.parseString('+(2), -(3), @(4)')
        self.assertEqual(len(result), 3)
        self.assertTrue(all([isinstance(x, action.Action) for x in result]))
        for parsed_action, op in zip(result, ["+", "-", "@"]):
            self.assertEqual(parsed_action._op._op_str, op)


    def test_action_str_equal(self):
        actions = ["+(2)", "-(3)", "@(4)"]
        parsed = [AP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        self.assertTrue(all([x == str(y) for x,y in zipped]))


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
