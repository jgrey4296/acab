#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
import pyparsing as pp

from acab.abstract.bootstrap_parser import BootstrapParser

class BootstrapParserTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        self.bp = BootstrapParser(empty=True)

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.bp)


    def test_add(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test", "awef")
        self.assertTrue(bool(self.bp))

    def test_query(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test", "awef")
        result = self.bp.query("test")
        self.assertIsNotNone(result)
        self.assertIsInstance(result, pp.ParserElement)
        self.assertEqual(str(result), '"awef"')

    def test_query_empty(self):
        self.assertFalse(bool(self.bp))
        with self.assertRaises(Exception):
            self.bp.query("*")

    def test_add_two(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test", "awef")
        self.assertEqual(len(self.bp), 1)
        self.bp.add("other", "blah")
        self.assertEqual(len(self.bp), 2)

    def test_query_two(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test", "awef")
        self.bp.add("other", "blah")

        result = self.bp.query("*")
        self.assertTrue("awef" in result.exprs)
        self.assertTrue("blah" in result.exprs)

    def test_add_chain(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test.chain", "awef")
        result = self.bp.query("test.chain")
        self.assertIsNotNone(result)
        self.assertEqual(str(result), '"awef"')

    def test_query_on_chain(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test.chain", "awef")
        result = self.bp.query("test.*")
        self.assertIsNotNone(result)
        self.assertEqual(str(result), '"awef"')

    def test_query_on_chain_multi_response(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test.chain", "awef")
        self.bp.add("test.other", "blah")
        result = self.bp.query("test.*")
        self.assertIsNotNone(result)
        self.assertTrue("awef" in result.exprs)
        self.assertTrue("blah" in result.exprs)

    def test_multi_add(self):
        self.assertFalse(bool(self.bp))
        self.bp.add("test.first", "awef",
                    "test.second", "blah")
        result = self.bp.query("test.*")
        self.assertIsNotNone(result)
        self.assertTrue("awef" in result.exprs)
        self.assertTrue("blah" in result.exprs)





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
