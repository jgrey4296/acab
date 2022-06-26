#!/usr/bin/env python3

import logging as logmod
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = logmod.getLogger(__name__)
##############################

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    import acab.modules.parsing.exlo.parsers.FactParser as FP
    import acab.modules.printing.printers as Printers
    from acab.core.config.config import AcabConfig
    from acab.core.printing import wrappers as PW
    from acab.core.util.sentences import ProductionComponent
    from acab.core.value.instruction import (Instruction, ProductionContainer,
                                            ProductionOperator)
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.printing import PrintSystem_i


class BasicPrintFunctionalityTests(unittest.TestCase):

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

    def test_basic(self):
        pass

    # sem system construction

    # override

    # handler registration

    # lookup

    # check

    # Test wrappers
