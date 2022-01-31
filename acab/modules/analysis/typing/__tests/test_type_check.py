import logging as root_logger
import unittest
import unittest.mock as mock
from functools import partial
from os.path import split, splitext

from acab import setup

config = setup()

from acab.core.data.acab_struct import AcabNode
from acab.core.data.default_structure import BIND
from acab.core.data.value import AcabValue, Sentence
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.analysis.typing import exceptions as TE
from acab.modules.analysis.typing.dsl import TypingDSL
from acab.modules.analysis.typing.unify import unify
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.modules.structures.trie.default import DEFAULT_TRIE
from acab.modules.analysis.typing.unify.util import gen_f
from acab.modules.analysis.typing.unify import type_unify_fns as tuf

# Set up the parser to ease test setup
dsl   = ppDSL.PyParseDSL()
dsl.register(EXLO_Parser).register(TypingDSL)
dsl.build()

# AcabReducible          : type_definition -> sentences with unique variable at head
# Sentence.add_prefix    : add prefix of unique var prior to unification with test node
# Typing is a statement. use Query : Type retrieval from WM, unify is an operator
# Use Rules for : Product / Sum / Operator Type differentiation

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
    def test_type_to_sentences(self):
        a_sen = dsl("a.test.sen(::def).sub.blah")[0]
        # remove initial prefix
        chopped = a_sen.remove_prefix(dsl("a.test")[0])
        type_ = dsl("def(::τ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens = type_.to_sentences()
        new_var = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        unified = tuf.type_unify(chopped, appended[0], CtxIns())
        result  = tuf.type_unify.apply(appended[0], unified)

        self.assertEqual(chopped, result)

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
        instr = dsl.parse_string("ᛦ λtypedef")

        # Go:
        walksem(instr, semsys)
