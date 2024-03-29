#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as logmod
logging = logmod.getLogger(__name__)


import acab
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()

class ReplParseTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        if bool(logmod.root.handlers):
            logmod.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    @unittest.skip("TODO")
    def test_repl_parse_init(self):
        pass

