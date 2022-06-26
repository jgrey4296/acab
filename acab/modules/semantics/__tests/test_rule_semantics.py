#!/opts/anaconda3/envs/ENV/python
"""
A Bare minimum template for testing with a set up acab engine
"""
import logging as logmod
import unittest
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock
from unittest.mock import create_autospec
import warnings

logging = logmod.getLogger(__name__)

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    import pyparsing as pp
    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)
    from acab.modules.engines.basic_engine import AcabBasicEngine
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.printing.default import DEFAULT_PRINTER
    from acab.modules.semantics.default import DEFAULT_SEMANTICS


class _ENGINE_TEST_TEMPLATE(unittest.TestCase):

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

        cls.eng = AcabBasicEngine(parser=EXLO_Parser,
                                  semantics=DEFAULT_SEMANTICS(),
                                  printer=DEFAULT_PRINTER(),
                                  modules=["acab.modules.operators.query", "acab.modules.operators.action"])

    @classmethod
    def tearDownClass(cls):
        logging.root.removeHandler(cls.file_h)

    def setUp(self):
        self.eng("~a")
        self.eng("~found")

    def test_initial(self):
        self.eng("""a.test.rule(::ρ):\n a.b.c?\n\n !! found\nend""")
        self.eng("a.b.c")
        self.eng(self.eng("a.test.$x?")[0].x)
        result = self.eng("found?")
        self.assertTrue(result)

    def test_basic_binding(self):
        self.eng("""a.test.rule(::ρ):\n a.b.$x?\n\n !! found.$x\nend""")
        ctx = self.eng("a.test.$x?")
        self.eng("a.b.c")
        self.eng(self.eng("a.test.$x?")[0].x)
        self.assertTrue(self.eng("found.c?"))

    def test_lexical_binding(self):
        self.eng("""a.test.rule(::ρ):\n a.b.$x?\n\n !! found.rule(::ρ):\n  a.b.$x?\n end\nend""")
        self.eng("a.b.c")
        self.eng(self.eng("a.test.$x?")[0].x)
        self.assertTrue(self.eng("found.rule?"))
        ctx = self.eng("found.$x?")
        self.assertEqual(ctx[0].x['QUERY'][0], "_:a.b.x")
