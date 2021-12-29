import logging as root_logger
import unittest
import unittest.mock as mock
from functools import partial
from os.path import split, splitext

from acab import setup

config = setup()

import acab.modules.analysis.typing.simple_unify_fns as suf
import acab.modules.analysis.typing.type_unify_fns as tuf
from acab.core.data.acab_struct import AcabNode
from acab.core.data.default_structure import BIND
from acab.core.data.value import AcabValue, Sentence
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.analysis.typing import type_exceptions as TE
from acab.modules.analysis.typing import unify
from acab.modules.analysis.typing.dsl import TypingDSL
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.modules.structures.trie.default import DEFAULT_TRIE

# Set up the parser to ease test setup
dsl   = ppDSL.PyParseDSL()
dsl.register(EXLO_Parser).register(TypingDSL)
dsl.build()

class TypeCheckTests(unittest.TestCase):

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


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    @unittest.skip("")
    def test_basic(self):
        semsys  = DEFAULT_SEMANTICS()
        walksem = DFSSemantics()
        ctxs    = ContextSet.build()

        decl    = dsl.parseString("a.test.def(::τ):\n sub.$x(::test)\nend")[0]
        asst    = dsl.parseString("a.value(::a.test.def).sub.blah")[0]

        # Insert into a wm
        semsys(decl)
        semsys(asst)

        # The Typecheck instruction:
        instr = dsl.pase_string("")

        # Go:
        walksem(instr, semsys)
