import unittest
import logging
import random
from test_context import pyRule
import pyRule.trie as T
import pyRule.trie.FactParser as FP
import pyRule.utils as util
import IPython

class Trie_Fact_Parser_Tests(unittest.TestCase):
    
    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_trivial(self):
        self.assertIsNotNone(FP.parseString)
        self.assertIsNotNone(FP.param_fact_string)
        self.assertIsNotNone(FP.param_fact_strings)
        
    def test_parseString(self):
        result = FP.parseString('.a.b.c')[0]
        self.assertIsInstance(result, list)
        self.assertTrue(all([isinstance(x, FP.Node) for x in result]))
        self.assertEqual("".join([str(x) for x in result]), '.a.b.c')

    def test_parseString(self):
        result = FP.parseString('.a.b.c,\n .b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, list) for x in result]))
 
    def test_param_fact_string(self):
        result = FP.param_fact_string.parseString('.a.b.$x')
        self.assertIsNotNone(result)

    def test_exclusion_operator_string_recovery(self):
        result = FP.parseString('!a!b!c')[0]
        self.assertEqual("".join([str(x) for x in result]), "!a!b!c")
        
    def test_numbers_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = round(random.random() * 1000)
            result = FP.parseString('.a.' + str(r))[0]
            self.assertEqual(result[-1]._value, r)

    def test_negative_number_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = - round(random.random() * mult)
            result = FP.parseString('.a.'+str(r))[0]
            self.assertEqual(result[-1]._value, r)

    def test_floats(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            a = round(random.random() * mult)
            b = round(random.random() * mult)
            float_form = float(str(a) + "." + str(b))
            d_form = str(a) + "d" + str(b)
            result = FP.parseString('.a.'+d_form)[0]
            self.assertEqual(result[-1]._value, float_form)

    def test_strings(self):
        result = FP.parseString('.a.b."This is a test"!c')[0]
        self.assertEqual(len(result), 4)
        self.assertEqual(result[2]._value, "This is a test")

    def test_bind_addition_to_node_recognition(self):
        result = FP.parseString('.$a.$b!$c')[0]
        for x in result:
            self.assertTrue(x._meta_eval[util.META_OP.BIND])

    def test_fact_leading_bind(self):
        result = FP.parseString('$x.a.b.c')[0]
        self.assertIsInstance(result[0], util.Bind)
        self.assertTrue(all([isinstance(x, T.Node) for x in result[1:]]))

            
    def test_fact_str_equal(self):
        actions = [".a.b.c", ".a.b!c", '.a.b."a string".c',
                   '.a.b!"a string"!c',
                   '.a.b.$x','.a!$x!y']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for a,p in zipped:
            pAsStr = "".join([str(x) for x in p])
            self.assertEqual(a,pAsStr)

    def test_leading_bind_str_equal(self):
        actions = ['$x.a.b.c', '$y!b.c', '$x.$y!$z']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for a,p in zipped:
            pAsStr = "".join([str(x) for x in p])
            self.assertEqual(a, pAsStr)


    def test_binding_expansion(self):
        bindings = { "a" : FP.parseString(".blah")[0], "b": FP.parseString(".bloo")[0] }
        result = FP.parseString('$a.b.$b!c')[0]
        expanded = util.expandFact(result, bindings)
        asString = "".join([str(x) for x in expanded if not x.is_root()])
        self.assertEqual(asString, ".blah.b.bloo!c")

    
        
if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_fact_parser_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
