"""
Check the components of type checking work, manually
"""
from __future__ import annotations

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
    from acab.core.data.acab_struct import AcabNode
    from acab.core.defaults.value_keys import BIND
    from acab.core.util.annotation import ValueAnnotation
    from acab.core.value.instruction import Instruction
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.context import ContextSet_i
    from acab.modules.analysis.typing import exceptions as TE
    from acab.modules.analysis.typing.module import TypeSpecFragment
    from acab.modules.analysis.typing.unify import type_unify_fns as tuf
    from acab.modules.analysis.typing.unify import unifier
    from acab.modules.analysis.typing.unify.util import gen_f
    from acab.modules.context.context_instance import MutableContextInstance
    from acab.modules.context.context_set import ContextInstance as CtxIns
    from acab.modules.operators.dfs.module import DFSExtension
    from acab.modules.operators.dfs.semantics import DFSSemantics
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.semantics.default import DEFAULT_SEMANTICS
    from acab.modules.structures.trie.default import DEFAULT_TRIE

ContextSet = config.prepare("Imports.Targeted", "context", actions=[config.actions_e.IMCLASS], args={"interface": ContextSet_i})()


class TypeCheckTests(unittest.TestCase):

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


        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser).register(TypeSpecFragment().build_dsl()).register(DFSExtension().build_dsl())
        cls.dsl.build()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    #----------
    def test_type_to_sentences(self):
        """
        Manually check the type and structure of a sentence against
        a definition.
        """
        a_sen    = self.dsl("a.test.sen(::def).sub.blah")[0]
        chopped  = a_sen.remove_prefix(self.dsl("a.test")[0])
        type_    = self.dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_[:]
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        unified  = tuf.type_unify(chopped, appended[0], CtxIns())
        result   = tuf.type_unify.apply(appended[0], unified)
        self.assertEqual(chopped, result)

    def test_typing_conflict(self):
        a_sen    = self.dsl("a.test.sen(::def).sub.blah(::bloo)")[0]
        # remove initial prefix
        chopped  = a_sen.remove_prefix(self.dsl("a.test")[0])
        type_    = self.dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_[:]
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(chopped, appended[0], CtxIns())

    def test_sub_typing(self):
        a_sen    = self.dsl("a.test.sen(::def).sub.blah(::test.bloo)")[0]
        # remove initial prefix
        chopped  = a_sen.remove_prefix(self.dsl("a.test")[0])
        type_    = self.dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_[:]
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        unified = tuf.type_unify(chopped, appended[0], CtxIns())
        result  = tuf.type_unify.apply(chopped, unified)
        self.assertEqual(result[-1].type, "_:test")

    def test_operator_typing(self):
        transform = self.dsl("transform(::χ):\n λa.b.c $x(::first) $y(::second) -> $z(::first)\nend")[0][0]
        op_def    = self.dsl("a.b.c(::λ): $g(::first) $h(::second) -> $i(::first)")[0][-1]
        # t -> sen
        t_sen = transform[0][1:].flatten()
        # def -> sen
        def_sen = op_def[0].flatten()
        # unify
        unified = tuf.type_unify(t_sen, def_sen, CtxIns())
        applied = tuf.type_unify.apply(t_sen, unified)
        self.assertEqual(applied[0][0][0].type, "_:first")
        self.assertEqual(applied[0][1][0].type, "_:second")
        self.assertEqual(applied[1][1].type, "_:first")

    def test_operator_typing2(self):
        transform = self.dsl("transform(::χ):\n λa.b.c $x(::first) $y -> $z(::first)\nend")[0][0]
        op_def    = self.dsl("a.b.c(::λ): $g(::first) $h(::second) -> $i(::first)")[0][-1]
        # t -> sen
        t_sen = transform[0][1:]
        # def -> sen
        def_sen = op_def[0]
        # unify
        unified = tuf.type_unify(t_sen, def_sen, CtxIns())
        applied = tuf.apply_types_onto_sen(t_sen, unified)

        self.assertEqual(applied[0][0][0].type, "_:first")
        self.assertEqual(applied[0][1][0].type, "_:second")
        self.assertEqual(applied[1][1].type, "_:first")

    def test_operator_typing_none_on_use(self):
        transform = self.dsl("transform(::χ):\n λa.b.c $x $y -> $z\nend")[0][0]
        op_def    = self.dsl("a.b.c(::λ): $g(::first) $h(::second) -> $i(::first)")[0][-1]
        # t -> sen
        t_sen = transform[0][1:].flatten()
        # def -> sen
        def_sen = op_def[0].flatten()
        # unify
        unified = tuf.type_unify(t_sen, def_sen, CtxIns())
        applied = tuf.apply_types_onto_sen(t_sen, unified)
        self.assertEqual(applied[0][0][0].type, "_:first")
        self.assertEqual(applied[0][1][0].type, "_:second")
        self.assertEqual(applied[1][1].type, "_:first")


    def test_operator_typing_conflict(self):
        transform = self.dsl("transform(::χ):\n λa.b.c $x(::first) $y(::other) -> $z(::first)\nend")[0][0]
        op_def    = self.dsl("a.b.c(::λ): $g(::first) $h(::second) -> $g(::first)")[0][-1]
        # t -> sen
        t_sen = transform[0][1:]
        # def -> sen
        def_sen = op_def[0]
        # unify
        with self.assertRaises(TE.TypeConflictException) as cm:
            ctx = tuf.type_unify(t_sen, def_sen, CtxIns())

        self.assertEqual(cm.exception.left, "y")
        self.assertEqual(cm.exception.right, "h")


    @unittest.skip("TODO")
    def test_query_condition_typechecking(self):
        """
        a.b.$x(∈ a.b.c)?

        ⊢ $x    : :ATOM
        ⊢ a.b.c : SENTENCE

        """
        pass

    @unittest.skip("TODO")
    def test_container_typing(self):
        pass

    @unittest.skip("TODO")
    def test_container_typing_conflict(self):
        pass



