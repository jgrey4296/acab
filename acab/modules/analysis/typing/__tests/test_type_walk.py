"""
Check trie walking
"""
import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from functools import partial
from os.path import split, splitext

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    from acab.core.parsing import pyparse_dsl as ppDSL

    from acab.interfaces.value import ValueFactory as VF
    from acab.interfaces import value as VI
    from acab.core.data.acab_struct import AcabNode
    from acab.core.defaults.value_keys import BIND
    from acab.core.value.instruction import Instruction
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.context import ContextSet_i
    from acab.modules.analysis.typing import exceptions as TE
    from acab.modules.analysis.typing.module import TypeSpecFragment, CheckStatementFragment
    from acab.modules.analysis.typing.unify import type_unify_fns as tuf
    from acab.modules.analysis.typing.unify import unifier
    from acab.modules.analysis.typing.unify.util import gen_f
    from acab.modules.context.context_instance import ContextInstance as CtxIns
    from acab.modules.context.context_instance import MutableContextInstance
    from acab.modules.engines.configured import exlo
    from acab.modules.operators.dfs.module import DFSExtension
    from acab.modules.operators.dfs.semantics import DFSSemantics
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.modules.semantics.default import DEFAULT_SEMANTICS
    from acab.modules.values.sen_val.module import Sen_Val_Parser, Sen_Val_Frag
    from acab.modules.structures.trie.default import DEFAULT_TRIE

ContextSet = config.prepare("Imports.Targeted", "context", actions=[config.actions_e.IMCLASS], args={"interface": ContextSet_i})()
default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

class TypeWalkTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h    = logmod.FileHandler(LOG_FILE_NAME, mode="w")
        cls.file_h.setLevel(LOGLEVEL)

        logging = logmod.getLogger(__name__)
        # logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        cls.logging = logging
        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        (cls.dsl.register(EXLO_Parser)
         .register(TypeSpecFragment().build_dsl())
         .register(DFSExtension().build_dsl())
         .register(Component_DSL)
         .register(Sen_Val_Parser))
        cls.dsl.build()

        cls.eng = exlo()
        cls.eng.load_modules(*default_modules, "acab.modules.operators.dfs.module", "acab.modules.analysis.typing")
        # cls.eng.load_modules("acab.modules.values.sen_val")

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        self.logging.info("---------- SETUP")
        self.eng("~test")
        self.eng("~found")
        self.eng("~types")
        self.eng("~acab")
        self.logging.info("---------- FINISHED SETUP")

    #----------
    def test_type_unify_walk(self):
        self.eng("""test.rule(::ρ):\n | $x |\n\n @x(::$y, ::$z)?\n $y?\n\n !! found.♭$z\nend""")
        self.eng("test.value(::types.blah)")
        self.eng("types.blah")
        walk_rule = self.dsl("walker(::ρ):\n test.$rules(::INSTRUCT.STRUCTURE.RULE)?\n\n !! found.rule\n ᛦ λ$rules\nend")[0][0]
        # trigger walk
        self.eng(walk_rule)
        # test
        self.assertTrue(self.eng("found.rule?"))
        self.assertTrue(self.eng("found.types.blah?"))

    def test_type_exists(self):
        # Check Instruction
        self.logging.info("Start")
        self.eng("""test.rule(::ρ):\n | $x |\n\n @x(::$y)?\n $y?\n\n ⊢ $x ∈ $y\n\n !! found.type.$y\nend""")
        # WM data
        self.eng("""test.value(::types.blah)""")
        self.eng("""types.blah(::τ)""")
        # Types
        # Walk Instruction
        walk_rule = self.dsl("walker(::ρ):\n test.$rules(::INSTRUCT.STRUCTURE.RULE)?\n\n !! found.rule\n ᛦ λ$rules\nend")[0][0]
        self.logging.info("----------")
        # trigger the walk
        self.eng(walk_rule)
        # test
        self.logging.info("----------")
        self.assertTrue(self.eng("found.rule?"))
        self.assertTrue(self.eng("found.type.types.blah?"))

    def test_type_exists_fail(self):
        # Check Instruction
        self.logging.info("Start")
        self.eng("""test.rule(::ρ):\n | $x |\n\n @x(::$y)?\n $y?\n\n ⊢ $x ∈ $y\n\n !! found.type.$y\nend""")
        # WM data
        self.eng("""test.value(::types.blah)""")
        # self.eng("""types.blah(::τ)""")
        # Types
        # Walk Instruction
        walk_rule = self.dsl("walker(::ρ):\n test.$rules(::INSTRUCT.STRUCTURE.RULE)?\n\n !! found.rule\n ᛦ λ$rules\nend")[0][0]
        self.logging.info("----------")
        # trigger the walk
        self.eng(walk_rule)
        # test
        self.logging.info("----------")
        self.assertTrue(self.eng("found.rule?"))
        self.assertFalse(self.eng("found.type.types.blah?"))

    def test_type_exists_structure(self):
        # Check Instruction
        self.logging.info("Start")
        self.eng("""test.rule(::ρ):\n | $x |\n\n @x(::$y)?\n $y?\n\n ⊢ @x ∈ $y\n\n !! found.type.$y\nend""")
        # WM data
        self.eng("""test.value(::types.blah)""")
        self.eng("test.value.a")
        self.eng("test.value.b")
        self.eng("""types.blah(::σ):\n a\n b\nend""")
        # Types
        # Walk Instruction
        walk_rule = self.dsl("walker(::ρ):\n test.$rules(::INSTRUCT.STRUCTURE.RULE)?\n\n !! found.rule\n ᛦ λ$rules\nend")[0][0]
        self.logging.info("----------")
        # trigger the walk
        self.eng(walk_rule)
        # test
        self.logging.info("----------")
        self.assertTrue(self.eng("found.rule?"))
        self.assertTrue(self.eng("found.type.types.blah?"))

    def test_type_exists_structure_fail(self):
        # Check Instruction
        self.logging.info("Start")
        self.eng("""test.rule(::ρ):\n | $x |\n\n @x(::$y)?\n $y?\n\n ⊢ $x ∈ $y\n\n !! found.type.$y\nend""")
        # WM data
        self.eng("""test.value(::types.blah)""")
        self.eng("""types.blah(::σ):\n a\n b\nend""")
        # Types
        # Walk Instruction
        walk_rule = self.dsl("walker(::ρ):\n test.$rules(::INSTRUCT.STRUCTURE.RULE)?\n\n !! found.rule\n ᛦ λ$rules\nend")[0][0]
        self.logging.info("----------")
        # trigger the walk
        self.eng(walk_rule)
        # test
        self.logging.info("----------")
        self.assertTrue(self.eng("found.rule?"))
        self.assertFalse(self.eng("found.type.types.blah?"))




    @unittest.skip("")
    def test_basic(self):
        semsys  = DEFAULT_SEMANTICS()
        walksem = DFSSemantics()
        ctxs    = ContextSet()

        decl    = self.dsl.parse_string("a.test.def(::τ):\n sub.$x(::test)\nend")[0]
        asst    = self.dsl.parse_string("a.value(::a.test.def).sub.blah")[0]

        # Insert into a wm
        semsys(decl)
        semsys(asst)

        # The Typecheck instruction:
        instr = self.dsl.parse_string("ᛦ λtypedef")

        # Go:
        walksem(instr, semsys)
