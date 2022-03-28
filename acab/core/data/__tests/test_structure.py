#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.interfaces.data import Structure_i, Node_i
from acab.core.data.acab_struct import BasicNodeStruct

# TODO
class StructureTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.WARNING)

        logging = root_logger.getLogger(__name__)
        logging.setLevel(root_logger.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)


    def test_creation(self):
        """ Check a structure can be created using the "build_default" method """
        struct = BasicNodeStruct.build_default()
        self.assertIsInstance(struct, Structure_i)
        self.assertIsInstance(struct.root, Node_i)
        self.assertIsInstance(struct.components, dict)
        # Empty except for root:
        self.assertFalse(bool(struct))
        # Root isn't counted
        self.assertEqual(len(struct), 0)
