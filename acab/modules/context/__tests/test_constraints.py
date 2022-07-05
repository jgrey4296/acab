#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from __future__ import annotations

import logging as logmod
import unittest
import warnings
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from unittest import mock

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    from acab.modules.context.constraints import ConstraintCollection
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.core.data.node import AcabNode
    from acab.interfaces.value import ValueFactory as VF
    from acab.modules.operators.query.query_operators import ELEM
    from acab.modules.context.context_set import ContextSet
    import acab.error.semantic as ASErr


class TestConstraintCollection(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        if bool(logging.root.handlers):
            logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_creation(self):
        pass

    def test_top_level_test(self):
        pass

    def test_constraint_sieve(self):
        pass
