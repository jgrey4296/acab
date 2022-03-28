#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging as logmod
logging = logmod.getLogger(__name__)

import acab
config = acab.setup()

from acab.interfaces.data import Structure_i, Node_i
from acab.core.data.acab_struct import BasicNodeStruct

# TODO
class StructureTests(unittest.TestCase):

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
