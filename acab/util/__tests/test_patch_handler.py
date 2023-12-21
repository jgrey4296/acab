import inspect
import logging as logmod
import unittest
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeVar,
                    cast)
from unittest import mock

logging = logmod.getLogger(__name__)

import warnings

import acab
from acab.interfaces.handler_system import Handler_i

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()
HandlerConfigSpec      = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": Handler_i})
config.override(HandlerConfigSpec, "acab.core.util.patch_handler.PatchHandler")

Handler = HandlerConfigSpec()

import acab.core.util.part_implementations.handler_system as HS
import acab.interfaces.handler_system as HSi
from acab.core.util.part_implementations.handler import BasicHandler
from acab.core.util.patch_handler import PatchHandler
from acab.error.handler import AcabHandlerException


class SimplestHandlerSystem(HS.HandlerSystem):

    def __call__(self, *args):
        pass

    def extend(self, *args):
        pass


class FrameInspector:
    """
    A simple test class to check the PatchHandler has sucessfully patched
    in the call method, reducing number of frames created
    """

    def __call__(self):
        return inspect.currentframe()


class TestHandlerSystem(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_config_override(self):
        a_handler = Handler("a_signal")
        self.assertIsInstance(a_handler, PatchHandler)
        self.assertIsInstance(a_handler, BasicHandler)

    def test_basic_setup(self):
        fn_mock   = mock.MagicMock()
        a_handler = Handler("a_signal", func=fn_mock)
        a_handler()
        fn_mock.assert_called()

    def test_frame_inspect(self):
        curr      = inspect.currentframe()
        fn        = FrameInspector()
        a_handler = Handler("a_signal", func=fn)
        result    = a_handler()
        self.assertEqual(curr.f_lineno, result.f_back.f_lineno)
        self.assertEqual(result.f_back.f_code.co_filename, __file__)

    def test_frame_inspect_fail(self):
        """
        Demonstrate the original handler adds an additional frame
        """
        curr      = inspect.currentframe()
        fn        = FrameInspector()
        a_handler = BasicHandler("a_signal", func=fn)
        result    = a_handler()
        self.assertNotEqual(curr.f_lineno, result.f_back.f_lineno)
        self.assertEqual(curr.f_lineno, result.f_back.f_back.f_lineno)
        self.assertNotEqual(result.f_back.f_code.co_filename, __file__)
        self.assertEqual(result.f_back.f_back.f_code.co_filename, __file__)
