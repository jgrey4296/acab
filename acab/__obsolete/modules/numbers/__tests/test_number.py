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

from acab.modules.values.numbers.parsing import NumberParser as NP

class NumberTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------

    def test_init(self):
        result = NP.NEG_NUM.parse_string("20")

    def test_init_2(self):
        result = NP.NEG_NUM.parse_string("-20")
