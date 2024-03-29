#!/usr/bin/env python3
"""
A Template for setting up a test case with an acab dsl ready
"""
from __future__ import annotations

import logging as logmod
import re
import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import acab
    config = acab.setup()
    # Uncomment this for indepth parse debugging
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

##############################


class _DSL_TEST_TEMPLATE(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h    = logmod.FileHandler(LOG_FILE_NAME, mode="w")
        cls.file_h.setLevel(LOGLEVEL)

        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.build()
        # Can now parse strings with self.dsl("a.b.c")

    @classmethod
    def tearDownClass(cls):
        logging.root.removeHandler(cls.file_h)


    # Add Tests here:


if __name__ == '__main__':
    unittest.main()
