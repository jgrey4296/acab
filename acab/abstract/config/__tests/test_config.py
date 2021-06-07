#!/usr/bin/env python3

#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as root_logger
import unittest
import unittest.mock as mock
from os.path import split, splitext

from acab import setup

from acab.abstract.config.config import AcabConfig

class ConfigTests(unittest.TestCase):

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

        def setUp(self):
            return 1

        def tearDown(self):
            return 1

        #----------
        # use testcase snippet
        # mock.Mock / MagicMock
        # create_autospec
        # @patch(' ') / with patch.object(...)

        # Test loading, getting,
        # directory loading
        # missing sections
        # missing keys

    def test_config_singleton(self):
        """ Check the config obj is a singleton"""
        config = AcabConfig.Get()
        self.assertIsInstance(config, AcabConfig)
        config2 = AcabConfig.Get()
        self.assertIs(config, config2)

    def test_config_value(self):
        """
        Check values can be retrieved
        """
        config = AcabConfig.Get()
        value = config.value("Data", "ROOT")
        self.assertEqual(value, "__root")


    def test_modal_spec(self):
        """ Check modal fields exist """
        config = AcabConfig.Get()
        self.assertTrue(config.enums)
        self.assertTrue(config.defaults)
        self.assertTrue(config.printing_extension)
        self.assertTrue(config.syntax_extension)
        # TODO Check values *in* the modal structures

    def test_config_prepare(self):
        """ Check values can be prepared """
        config = AcabConfig.Get()
        prep_tuple = config.prepare("Data", "ROOT")
        self.assertIsInstance(prep_tuple, tuple)
        self.assertEqual(len(prep_tuple), 5)

    def test_config_value_missing(self):
        """ Check error is thrown for missing value """
        config = AcabConfig.Get()
        with self.assertRaises(Exception):
            config.value("blah", "bloo")

    def test_config_prepare_missing(self):
        """ Check config errors if you prepare
        a missing value """
        config = AcabConfig.Get()
        with self.assertRaises(Exception):
            config.prepare("blah", "bloo")

    def test_modal_spec_missing(self):
        """
        Check config errors when you try to use missing modal values
        """
        config = AcabConfig.Get()
        with self.assertRaises(Exception):
            config.modal_enums['blah']


    # -> ClosedSet[Values, Node]
    # Creation,
