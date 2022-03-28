#!/usr/bin/env python3
import unittest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock

import acab

acab.setup()

from acab.core.engine.module_loader import ModuleLoader


class TestModuleLoader(unittest.TestCase):

    def test_initial_ModuleLoader(self):
        ml = ModuleLoader()
        self.assertIsNotNone(ml)

    def test_load_module(self):
        pass

    def test_extract_from_module(self):
        pass

    def test_contains(self):
        pass


if __name__ == '__main__':
    unittest.main()
