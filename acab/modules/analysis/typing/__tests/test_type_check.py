import logging as logmod
import unittest
import unittest.mock as mock
from functools import partial
from os.path import split, splitext

from acab import setup

config = setup()

from acab.core.data.acab_struct import AcabNode
from acab.core.data.default_structure import BIND
from acab.core.data.value import AcabValue
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.analysis.typing import exceptions as TE
from acab.modules.analysis.typing.dsl import TypingDSL
from acab.modules.analysis.typing.unify import unifier
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.context.context_set import ContextSet, MutableContextInstance
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.modules.structures.trie.default import DEFAULT_TRIE
from acab.modules.analysis.typing.unify.util import gen_f
from acab.modules.analysis.typing.unify import type_unify_fns as tuf
from acab.modules.operators.dfs.module import DFSQueryDSL

# Set up the parser to ease test setup
dsl   = ppDSL.PyParseDSL()
dsl.register(EXLO_Parser).register(TypingDSL).register(DFSQueryDSL)
dsl.build()

# AcabReducible          : type_definition -> sentences with unique variable at head
# Sentence.add_prefix    : add prefix of unique var prior to unification with test node
# Typing is a statement. use Query : Type retrieval from WM, unify is an operator
# Use Rules for : Product / Sum / Operator Type differentiation

class TypeCheckTests(unittest.TestCase):

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


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_type_to_sentences(self):
        """
        Manually check the type and structure of a sentence against
        a definition.
        """
        a_sen    = dsl("a.test.sen(::def).sub.blah")[0]
        chopped  = a_sen.remove_prefix(dsl("a.test")[0])
        type_    = dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_.to_sentences()
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        unified  = tuf.type_unify(chopped, appended[0], CtxIns())
        result   = tuf.type_unify.apply(appended[0], unified)
        self.assertEqual(chopped, result)

    def test_typing_conflict(self):

        a_sen    = dsl("a.test.sen(::def).sub.blah(::bloo)")[0]
        # remove initial prefix
        chopped  = a_sen.remove_prefix(dsl("a.test")[0])
        type_    = dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_.to_sentences()
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(chopped, appended[0], CtxIns())

    def test_sub_typing(self):
        a_sen    = dsl("a.test.sen(::def).sub.blah(::test.bloo)")[0]
        # remove initial prefix
        chopped  = a_sen.remove_prefix(dsl("a.test")[0])
        type_    = dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_.to_sentences()
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        unified = tuf.type_unify(chopped, appended[0], CtxIns())
        result  = tuf.type_unify.apply(chopped, unified)
        self.assertEqual(result[-1].type, "_:test")


    def test_operator_typing(self):
        transform = dsl("transform(::χ):\n λa.b.c $x(::first) $y(::second) -> $z(::first)\nend")[0][0]
        op_def    = dsl("a.b.c(::λ): $g(::first).$h(::second).$i(::first)")[0][-1]
        # t -> sen
        t_sen = transform.to_sentences()[0][1]
        self.assertEqual(t_sen.name, "Params")
        # def -> sen
        def_sen = op_def.to_sentences()[0]
        # unify
        unified = tuf.type_unify(t_sen, def_sen, CtxIns())
        self.assertEqual(unified.x, "g")
        self.assertEqual(unified.y, "h")


    def test_operator_typing_conflict(self):
        transform = dsl("transform(::χ):\n λa.b.c $x(::first) $y(::other) -> $z(::first)\nend")[0][0]
        op_def    = dsl("a.b.c(::λ): $g(::first).$h(::second).$i(::first)")[0][-1]
        # t -> sen
        t_sen = transform.to_sentences()[0][1]
        self.assertEqual(t_sen.name, "Params")
        # def -> sen
        def_sen = op_def.to_sentences()[0]
        # unify
        with self.assertRaises(TE.AcabUnifyVariableInconsistencyException):
            tuf.type_unify(t_sen, def_sen, CtxIns())


    @unittest.skip("TODO")
    def test_query_condition_typechecking(self):
       pass

    @unittest.skip("TODO")
    def test_container_typing(self):
        pass

    @unittest.skip("TODO")
    def test_container_typing_conflict(self):
        pass



