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
    def test_construction(self):
        return

    def test_length(self):
        return

    def test_eq(self):
        return

    def test_iter(self):
        return

    def test_get_item(self):
        return

    def test_expand_bindings(self):
        return

    def test_copy(self):
        return

    def test_add(self):
        return

    def test_data(self):
        return


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
