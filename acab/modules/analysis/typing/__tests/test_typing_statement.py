"""
Test the typing checking extension
"""
import logging as logmod
import unittest
import warnings
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from unittest import mock

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        # DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    import acab.interfaces.value as VI
    from acab.core.data.node import AcabNode
    from acab.core.defaults import value_keys as DS
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.interfaces import fragments as FI
    from acab.interfaces.context import ContextSet_i
    from acab.modules.analysis.typing import exceptions as TE
    from acab.modules.analysis.typing.module import (CheckStatementFragment,
                                                     TypeSpecFragment)
    from acab.modules.analysis.typing.unify import type_unify_fns as tuf
    from acab.modules.operators.query import ELEM, EQ, SimpleTypeMatch
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.printing.default import DEFAULT_PRINTER
    from acab.modules.semantics.default import DEFAULT_SEMANTICS
    from acab.modules.values.binding.variable_control import rectx

CtxSet = config.prepare("Imports.Targeted", "context", actions=[config.actions_e.IMCLASS], args={"interface": ContextSet_i})()

class TestTypingStatement(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.register(TypingFragment().build_dsl())

        cls.dsl.build()


    def test_initial(self):
        """ The Fragment can be created """
        instance = CheckStatementFragment()
        self.assertIsInstance(instance, CheckStatementFragment)
        self.assertTrue(hasattr(instance, "signal"))

    def test_dsl_build(self):
        """ The dsl can be built """
        instance = CheckStatementFragment()
        dsl_frag = instance.build_dsl()
        self.assertIsInstance(dsl_frag, FI.DSL_Fragment_i)

    def test_printer_build(self):
        """ The printer can be built """
        instance = CheckStatementFragment()
        print_frag = instance.build_printers()
        self.assertIsInstance(print_frag, FI.Printer_Fragment_i)

    def test_semantic_build(self):
        """ The semantics can be built """
        instance = CheckStatementFragment()
        sem_frag = instance.build_semantics()
        self.assertIsInstance(sem_frag, FI.Semantic_Fragment_i)

    def test_dsl(self):
        """ The dsl builds a sentence correctly """
        instance = CheckStatementFragment()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        result = dsl_frag.handlers[0]("⊢ a.b.c ∈ d.e.f")[0]
        self.assertIsInstance(result, VI.Sentence_i)
        self.assertEqual(result.data[DS.SEMANTIC_HINT], "TYPE_CHECK")
        self.assertIn("loc", result)
        self.assertIn("def", result)
        self.assertIsInstance(result['loc'], VI.Sentence_i)
        self.assertIsInstance(result['def'], VI.Sentence_i)

    def test_print(self):
        """ The Printer works  """
        print_sys = DEFAULT_PRINTER()
        instance = CheckStatementFragment()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        instr = dsl_frag.handlers[0]("⊢ a.b.c ∈ d.e.f")[0]

        printer  = instance.build_printers()
        print_sys.register(printer)
        result = print_sys.pprint(instr)
        self.assertEqual(result, "⊢ a.b.c ∈ d.e.f")

    def test_print2(self):
        """ The printer works on exclusion sentences """
        print_sys = DEFAULT_PRINTER()
        instance = CheckStatementFragment()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        instr = dsl_frag.handlers[0]("⊢ a.different!sen ∈ blah!bloo")[0]

        printer  = instance.build_printers()
        print_sys.register(printer)
        result = print_sys.pprint(instr)
        self.assertEqual(result, "⊢ a.different!sen ∈ blah!bloo")


    def test_print_vars(self):
        """ The printer works on @vars"""
        print_sys = DEFAULT_PRINTER()
        instance = CheckStatementFragment()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        instr = dsl_frag.handlers[0]("⊢ @x ∈ $y")[0]

        printer  = instance.build_printers()
        print_sys.register(printer)
        result = print_sys.pprint(instr)
        self.assertEqual(result, "⊢ @x ∈ $y")

    def test_semantics_simple(self):
        """
        Check a simple type checks
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.other(::a.def).sub.blah")[0]
        type_ = self.dsl("a.def(::σ):\n sub.$x(::test)\nend")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "==": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        result = semsys(instr, ctxs=ctxs)


    def test_semantics(self):
        """
        Check a subtrie type checks
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.sen(::a.def).sub.blah")[0]
        sen_2 = self.dsl("a.test.sen(::a.def).other.awg(::blah)")[0]
        type_ = self.dsl("a.def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(sen_2)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "==": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        result = semsys(instr, ctxs=ctxs)

    def test_semantics_fail(self):
        """
        Check a badly typed sentence complains
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.sen(::a.def).sub.blah(::bad)")[0]
        sen_2 = self.dsl("a.test.sen(::a.def).other.awg(::blah)")[0]
        type_ = self.dsl("a.def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(sen_2)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "==": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        with self.assertRaises(TE.AcabTypingException):
            semsys(instr, ctxs=ctxs)

    def test_semantics_fail_2(self):
        """

        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.sen(::a.def).sub.blah")[0]
        sen_2 = self.dsl("a.test.sen(::a.def).other.awg(::bloo)")[0]
        type_ = self.dsl("a.def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(sen_2)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "==": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        with self.assertRaises(TE.AcabTypingException):
            semsys(instr, ctxs=ctxs)

    def test_semantics_subtype(self):
        """
        Check a subtype matches a supertype
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.sen(::a.def).sub.blah")[0]
        sen_2 = self.dsl("a.test.sen(::a.def).other.awg(::blah.bloo)")[0]
        type_ = self.dsl("a.def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(sen_2)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "==": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        semsys(instr, ctxs=ctxs)

    def test_semantics_subtype_matching(self):
        """

        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.sen(::a.def).sub.blah(::test)")[0]
        sen_2 = self.dsl("a.test.sen(::a.def).other.awg(::blah.bloo)")[0]
        type_ = self.dsl("a.def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(sen_2)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "τ=": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(::TYPE_DEF)?")[0], ctxs=ctxs)
        semsys(instr, ctxs=ctxs)


    def test_semantics_subtype_matching2(self):
        """
        Check
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        a_sen = self.dsl("a.test.sen(::a.def).sub.blah(::test)")[0]
        type_ = self.dsl("a.def(::τ)")[0]
        instr = self.dsl['transform.statement'].parse_string('⊢ @x ∈ $y')[0]

        semsys(a_sen)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "τ=": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(::TYPE_DEF, ∈ def)?")[0], ctxs=ctxs)

        result = semsys(instr, ctxs=ctxs)

    def test_semantics_operator_matching(self):
        """
        Check
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        trans_sen = self.dsl("a.test.transform(::χ): \n λan.op.def test.sen val.$var -> $y \nend")[0]
        type_     = self.dsl("an.op.def(::λ): $x $y $z")[0]
        instr     = self.dsl['transform.statement'].parse_string('⊢ @x')[0]

        semsys(trans_sen)
        semsys(type_)

        ctxs = CtxSet(CtxSet.instance_constructor(data={"∈": ELEM(), "τ=": SimpleTypeMatch()}))

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("an.op.$y?")[0], ctxs=ctxs)

        breakpoint()
        result = semsys(instr, ctxs=ctxs)
        pass


    def test_instructions_to_sentences(self):
        result = self.dsl("a.test.rule(::ρ):\n a.b.c?\n d.e.f?\n\n λx.y.z $x $y -> $z\n\n !! a.b.c\nend")[0]
        result2 = self.dsl("a.test.rule(::ρ):\n λx.y.z a.b.$x z.y.q.$y -> $z\n\n !! a.b.c\nend")[0]
        breakpoint()


if __name__ == '__main__':
    unittest.main()
