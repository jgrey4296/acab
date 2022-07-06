#!/usr/bin/env python3
# mypy: disallow-untyped-defs=False
from __future__ import annotations

import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from enum import Enum, EnumMeta
from importlib.resources import files
from os.path import join, split, splitext
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import acab
    from acab.core.config.config import (AcabConfig, ConfigSingletonMeta,
                                         ConfigSpec)
    from acab.core.util.log_formatter import AcabLogRecord
    from acab.error.config import AcabConfigException
    AcabLogRecord.install()


class ConfigTests(unittest.TestCase):
    config : acab.types.Config

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

        cls.existing_config = getattr(ConfigSingletonMeta, "_instance", None)
        setattr(ConfigSingletonMeta, "_instance", None)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)
        # Manual singleton overriding
        setattr(ConfigSingletonMeta, "_instance", cls.existing_config)

    def setUp(self):
        data_path = files("acab.core.config.__tests")
        config_file = data_path.joinpath("basic.config")
        self.config = AcabConfig(config_file, build=True)

    def tearDown(self):
        self.config = None
        ConfigSingletonMeta._instance = None

    def test_config_singleton(self):
        """ Check the config obj is a singleton"""
        config = AcabConfig()
        self.assertIsInstance(config, AcabConfig)
        config2 = AcabConfig()
        self.assertIs(config, config2)

    def test_config_value(self):
        """
        Check values can be retrieved
        """
        config = AcabConfig()
        spec   = config.prepare("Handler.System", "DEFAULT_SIGNAL")
        value  = config.value(spec)
        self.assertEqual(value, "test")

    def test_config_enums(self):
        """
        Check values can be retrieved
        """
        config = AcabConfig()
        spec   = config.prepare("Enum.Test", "bob", _type=Enum)
        value  = config.value(spec)
        self.assertIsNotNone(value)
        self.assertEqual(value.name, "bob")
        self.assertIn(spec.section, config.enums)
        self.assertIn(spec.key, config.enums[spec.section].__members__)

    def test_config_prepare(self):
        """ Check values can be prepared """
        config = AcabConfig()
        prep_tuple = config.prepare("Handler.System", "DEFAULT_SIGNAL")
        self.assertIsInstance(prep_tuple, ConfigSpec)

    def test_config_prepare_missing(self):
        """ Check config errors if you prepare
        a missing value """
        config = AcabConfig()
        with self.assertRaises(AcabConfigException):
            config.prepare("blah", "bloo")

    def test_spec_type(self):
        bool_val = self.config.prepare("Handler.System", "OTHER", _type=bool)()
        self.assertIsInstance(bool_val, bool)

    @unittest.expectedFailure
    def test_spec_missing_type(self):
        with self.assertWarns(UserWarning):
            self.config.prepare("Handler.System", "OTHER", _type=float)

    def test_spec_list(self):
        list_val = self.config.prepare("List.Section", _type=list)()
        self.assertIsInstance(list_val, list)
        self.assertEqual(len(list_val), 3)

    def test_spec_default_type(self):
        def_val = self.config.prepare("List.Section", "bob")()
        self.assertIsInstance(def_val, str)

    @unittest.expectedFailure
    def test_spec_str_type(self):
        with self.assertWarns(UserWarning):
            def_val = self.config.prepare("List.Section", "bob", _type=str)()
            self.assertIsInstance(def_val, str)

    def test_spec_dict_type(self):
        dict_val = self.config.prepare("Dict.Section", _type=dict)()
        self.assertIsInstance(dict_val, dict)
        self.assertEqual(len(dict_val), 3)

    def test_spec_enum_type(self):
        enum_val = self.config.prepare("List.Section", _type=Enum)()
        self.assertIsInstance(enum_val, EnumMeta)
        self.assertIn("bob", enum_val.__members__)

    def test_spec_list_subactions(self):
        mod_val = self.config.prepare("Sub.Action.Test", _type=list, actions=[self.config.actions_e.STRIPQUOTE])()
        unmod_val = self.config.prepare("Sub.Action.Test", _type=list)()
        self.assertEqual(mod_val[0], "blah")
        self.assertNotEqual(unmod_val[0], "blah")

    def test_type_extension(self):
        self.assertNotIn(float, self.config.type_actions)
        def float_action(ret_section:Any, actions=None, spec=None, config=None) -> Any:
            value = ret_section[spec.key]
            return float(value)

        self.config.type_actions[float] = float_action
        float_val = self.config.prepare("Float.Test", "a_val", _type=float)()
        self.assertIsInstance(float_val, float)

    def test_without_type_extension(self):
        float_val = self.config.prepare("Float.Test", "a_val")()
        self.assertIsInstance(float_val, str)

    def test_missing_spec_with_default(self):
        with self.assertWarns(UserWarning):
            spec = self.config.prepare("missing", default="Test")
            self.assertEqual(spec(), "Test")

    def test_actual_spec_with_default(self):
        spec = self.config.prepare("Handler.System", "DEFAULT_SIGNAL", default="some val")
        self.assertEqual(spec(), "test")

    def test_missing_spec_default_skips_actions(self):
        with self.assertWarns(UserWarning):
            spec = self.config.prepare("missing", default='"Test"', actions=[self.config.actions_e.STRIPQUOTE])
            self.assertEqual(spec(), '"Test"')
