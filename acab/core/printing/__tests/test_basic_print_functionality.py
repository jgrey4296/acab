#!/usr/bin/env python3

import logging as logmod
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = logmod.getLogger(__name__)
##############################

import acab

config = acab.setup()

import acab.modules.parsing.exlo.parsers.FactParser as FP
import acab.modules.printing.printers as Printers
from acab.core.config.config import AcabConfig
from acab.core.value.instruction import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator)
from acab.core.value.value import AcabValue
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.printing import PrintSystem_i
from acab.core.printing import wrappers as PW

class BasicPrintFunctionalityTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

    def test_basic(self):
        pass

    # sem system construction

    # override

    # handler registration

    # lookup

    # check

    # Test wrappers
