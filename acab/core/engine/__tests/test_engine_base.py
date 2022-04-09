#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging as logmod


class EngineBaseTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        logmod.getLogger('').setLevel(logmod.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = logmod.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(logmod.DEBUG)

        console = logmod.StreamHandler()
        console.setLevel(logmod.WARNING)

        logging = logmod.getLogger(__name__)
        logging.setLevel(logmod.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)


    def test_init(self):
        pass

    def test_load_file(self):
        pass

    def test_save_file(self):
        pass

    def test_to_sentences(self):
        pass

    def test_pprint(self):
        pass

    def test_load_modules(self):
        pass

    def test_call(self):
        pass
