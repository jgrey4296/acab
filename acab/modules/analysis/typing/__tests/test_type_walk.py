import logging as logmod
import unittest
import unittest.mock as mock
from functools import partial
from os.path import split, splitext

from acab import setup

config = setup()

from acab.core.data.acab_struct import AcabNode
from acab.core.value.default_structure import BIND
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.analysis.typing import exceptions as TE
from acab.modules.analysis.typing.module import TypingDSL
from acab.modules.analysis.typing.unify import type_unify_fns as tuf
from acab.modules.analysis.typing.unify import unifier
from acab.modules.analysis.typing.unify.util import gen_f
from acab.modules.context.context_instance import ContextInstance as CtxIns
from acab.modules.context.context_instance import MutableContextInstance
from acab.modules.context.context_set import ContextSet
from acab.modules.engines.configured import exlo
from acab.modules.operators.dfs.module import DFS_DSL
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.modules.structures.trie.default import DEFAULT_TRIE

default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

dsl = None

class TypeWalkTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        global dsl
        # Set up the parser to ease test setup
        dsl   = ppDSL.PyParseDSL([], [], [])
        dsl.register(EXLO_Parser).register(TypingDSL).register(DFS_DSL)
        dsl.build()

        cls.eng = exlo()
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs.module", "acab.modules.analysis.typing")


    def setUp(self):
        self.eng("~additional")
        self.eng("~walker")
        self.eng("~types")
        self.eng("~found")
        self.eng("~acab")
        self.eng("~test")


    #----------
    def test_walk(self):
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x(::$y)?\n\n !! found.$y\nend """)

        walk_rule = dsl("walker(::ρ):\n walker.$rules?\n\n !! test.run.$rules\n ᛦ λ$rules\nend")[0][0]
        # trigger walk
        self.eng(walk_rule)
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 2)

    def test_walk2(self):
        # add rule
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x(::$y)?\n\n !! found.$y\nend""")
        self.eng("additional.test(::blah)")
        walk_rule = dsl("walker(::ρ):\n walker.$rules?\n\n !! test.run.$rules\n ᛦ λ$rules\nend")[0][0]
        # trigger walk
        self.eng(walk_rule)
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 3)

    def test_type_unify_walk(self):
        self.eng("~acab")
        self.eng("""walker.rule(::ρ):\n | $x |\n\n @x(::$y)?\n types.$y?\n\n !! found.$y\nend""")
        self.eng("additional.test(::blah)")
        self.eng("types.blah")
        walk_rule = dsl("walker(::ρ):\n walker.$rules?\n\n !! test.run.rule\n ᛦ λ$rules\nend")[0][0]
        # trigger walk
        self.eng(walk_rule)
        # test
        ctx_results = self.eng("found.$x?")
        self.assertEqual(len(ctx_results), 1)

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
