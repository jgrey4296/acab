import logging as logmod
import unittest
import unittest.mock as mock
from functools import partial
from os.path import split, splitext

from acab import setup

config = setup()

from acab.core.data.acab_struct import AcabNode
from acab.core.data.default_structure import BIND
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.data.value import AcabValue
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.analysis.typing import exceptions as TE
from acab.modules.analysis.typing.dsl import TypingDSL
from acab.modules.analysis.typing.unify import type_unify_fns as tuf
from acab.modules.analysis.typing.unify import unifier
from acab.modules.analysis.typing.unify.util import gen_f
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.engines.configured import exlo
from acab.modules.operators.dfs.module import DFSQueryDSL
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.modules.structures.trie.default import DEFAULT_TRIE

default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

dsl = None
walk_rule = dsl("walker(::ρ):\n walker.$rules?\n\n !! test.run.$rules\n ᛦ λ$rules\nend")[0][0]

class TypeWalkTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global logging
        logmod.getLogger('').setLevel(logmod.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = logmod.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(logmod.DEBUG)

        console = logmod.StreamHandler()
        console.setLevel(logmod.WARNING)

        logging = logmod.getLogger(__name__)
        logging.setLevel(logmod.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)

        global dsl
        # Set up the parser to ease test setup
        dsl   = ppDSL.PyParseDSL()
        dsl.register(EXLO_Parser).register(TypingDSL).register(DFSQueryDSL)
        dsl.build()

        cls.eng = exlo()
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs.module", "acab.modules.analysis.typing")


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_walk(self):
        # add rule
        self.eng("""
walker.rule(::ρ):
  | $x |

  $x(::$y)?

  !! has.type.$y
end
""")
        # trigger walk
        breakpoint()
        self.eng(walk_rule)
        # test

        self.assertEqual(2, 2)

    @unittest.skip("")
    def test_basic(self):
        semsys  = DEFAULT_SEMANTICS()
        walksem = DFSSemantics()
        ctxs    = ContextSet()

        decl    = dsl.parse_string("a.test.def(::τ):\n sub.$x(::test)\nend")[0]
        asst    = dsl.parse_string("a.value(::a.test.def).sub.blah")[0]

        # Insert into a wm
        semsys(decl)
        semsys(asst)

        # The Typecheck instruction:
        instr = dsl.parse_string("ᛦ λtypedef")

        # Go:
        walksem(instr, semsys)
