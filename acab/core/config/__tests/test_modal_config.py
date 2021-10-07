from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
import unittest
import unittest.mock as mock
from os.path import split, splitext

from acab import setup

from acab.core.config.config import AcabConfig, ConfigSpec
from acab.error.config_exception import AcabConfigException

class ModalConfigTests(unittest.TestCase):

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

        # Setup default config with default files
        cls.config = setup()

    def test_modal_spec_missing(self):
        """
        Check config errors when you try to use missing modal values
        """
        config = AcabConfig.Get()
        with self.assertRaises(Exception):
            config.enums['blah']

    def test_modal_spec(self):
        """ Check modal fields exist """
        config = AcabConfig.Get()
        self.assertTrue(config.enums)
        self.assertTrue(config.defaults)
        self.assertTrue(config.printing_extension)
        self.assertTrue(config.syntax_extension)
        # TODO Check values *in* the modal structures
