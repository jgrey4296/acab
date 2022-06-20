import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

from acab.core.config.config import AcabConfig, ConfigSingletonMeta, ConfigSpec
from acab.error.config import AcabConfigException


class ModalConfigTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

        # Setup default config with default files
        cls.config = config

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def test_modal_spec_missing(self):
        """
        Check config errors when you try to use missing modal values
        """
        config = AcabConfig()
        with self.assertRaises(Exception):
            config.enums['blah']

    def test_modal_spec(self):
        """ Check modal fields exist """
        config = AcabConfig()
        self.assertTrue(config.enums)
        self.assertTrue(config.defaults)
        self.assertTrue(config.printing_extension)
        self.assertTrue(config.syntax_extension)
        # TODO Check values *in* the modal structures
