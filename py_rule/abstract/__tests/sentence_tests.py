#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging


class SentenceTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    @unittest.skip('TODO')
    def test_construction(self):
        return

    @unittest.skip('TODO')
    def test_length(self):
        return

    @unittest.skip('TODO')
    def test_eq(self):
        return

    @unittest.skip('TODO')
    def test_iter(self):
        return

    @unittest.skip('TODO')
    def test_get_item(self):
        return

    @unittest.skip('TODO')
    def test_bind(self):
        return

    @unittest.skip('TODO')
    def test_copy(self):
        return

    @unittest.skip('TODO')
    def test_add(self):
        return

    @unittest.skip('TODO')
    def test_data(self):
        return


    @unittest.skip("TODO")
    def test_slice(self):
        pass

    @unittest.skip("TODO")
    def test_attach_statement(self):
        pass

    @unittest.skip("TODO")
    def test_detach_statement(self):
        pass




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
