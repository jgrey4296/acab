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
from unittest.case import skip

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
        cls.dsl.register(TypeSpecFragment().build_dsl())
        cls.dsl.register(CheckStatementFragment().build_dsl())

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
        result = dsl_frag.handlers[0].parse_string("⊢ a.b.c ∈ d.e.f")[0]
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
        instr = dsl_frag.handlers[0].parse_string("⊢ a.b.c ∈ d.e.f")[0]

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
        instr = dsl_frag.handlers[0].parse_string("⊢ a.different!sen ∈ blah!bloo")[0]

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
        instr = dsl_frag.handlers[0].parse_string("⊢ @x ∈ $y")[0]

        printer  = instance.build_printers()
        print_sys.register(printer)
        result = print_sys.pprint(instr)
        self.assertEqual(result, "⊢ @x ∈ $y")

    def test_op_simple_check(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y -> $z")[0]

        trans_use = self.dsl['transform.core'].parse_string("λan.op.def test.sen val.$var -> $y")[0]
        trans_sen = trans_use[1:]

        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())
        self.assertEqual(ctx['x__type'], "_:test.sen")
        self.assertEqual(ctx['y__type'], "_:val.var")
        self.assertEqual(ctx.y, 'z__type')

    def test_op_call(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x(::SENTENCE.blah) $y(::SENTENCE.bloo) -> $z(::aweg)")[0]

        trans_use = self.dsl['transform.core'].parse_string("λan.op.def test.sen val.$var -> $y")[0]
        trans_sen = trans_use[1:]
        mock_ctx = CtxSet()
        mock_ctx[0].set_current_node(AcabNode(trans_def))
        semsys_mock = mock.MagicMock()
        semsys_mock.return_value = mock_ctx
        ctx = CheckStatementFragment().op_check(trans_use, semsys_mock, ctxs=CtxSet())
        self.assertEqual(ctx['x__type'], "_:test.sen")
        self.assertEqual(ctx['y__type'], "_:val.var")
        self.assertEqual(ctx.y, 'z__type')

    def test_op_param_apply_check(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x(::blah) $y(::bloo) -> $z")[0]

        trans_use = self.dsl['transform.core'].parse_string("λan.op.def $a $b -> $y")[0]
        trans_sen = trans_use[1:]
        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())

        self.assertEqual(ctx.a.type, "_:blah")
        self.assertEqual(ctx.b.type, "_:bloo")
        self.assertEqual(ctx.y.type, "_:ATOM")


    def test_op_type_return_check(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y -> $z(::blah)")[0]

        trans_use = self.dsl['transform.core'].parse_string("λan.op.def test.sen val.$var -> $y")[0]
        trans_sen = trans_use[1:]
        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())
        self.assertEqual(ctx[str(trans_sen.flatten())].value, trans_def[0][1][1].type)
        self.assertEqual(ctx.y.type, "_:blah")

    def test_op_type_no_return(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y")[0]

        trans_use = self.dsl['action.core'].parse_string("λan.op.def test.sen val.$var")[0]
        trans_sen = trans_use[1:]
        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())
        self.assertEqual(ctx.x__type, "_:test.sen")
        self.assertEqual(ctx.y__type, "_:val.var")

    def test_op_type_return_use_specified(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y -> $z")[0]

        trans_use = self.dsl['transform.core'].parse_string("λan.op.def test.sen val.$var -> $y(::blah)")[0]
        trans_sen = trans_use[1:]
        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())
        self.assertEqual(ctx[trans_sen.key()].value, "_:blah")


    def test_op_type_return_check_fail(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y -> $z(::blah)")[0]

        trans_use = self.dsl['transform.core'].parse_string("λan.op.def test.sen val.$var -> $y(::bloo)")[0]
        trans_sen = trans_use[1:]
        with self.assertRaises(TE.AcabUnifyGroupException):
            CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())

    def test_structural_check(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b.$x")[0]

        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertEqual(ctx.x__type, "c")

    def test_structural_check_other_direction(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.c")[0]

        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertEqual(ctx.x, "c")


    def test_structural_check2(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b.$x(::test)")[0]

        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertIn("[a.b.c]", ctx)
        self.assertEqual(ctx.x__type,  "c")
        self.assertEqual(ctx['[a.b.c]'].value, "_:test")

    def test_structural_bind_entire(self):
        sen1 = self.dsl("$a")[0]
        sen2 = self.dsl("a.b.$x(::test)")[0]
        sen2_re = rectx(sen2, ctx={}, name_suff="_type")
        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertEqual(ctx.a, sen2_re)

    def test_structural_fail(self):
        sen1 = self.dsl("a.b.c(::not.test)")[0]
        sen2 = self.dsl("a.b.$x(::test)")[0]

        with self.assertRaises(TE.AcabUnifyGroupException):
            CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())

    def test_generalisation(self):
        sen1 = self.dsl("a.b.c(::test.blah)")[0]
        sen2 = self.dsl("a.b.$x(::test)")[0]

        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertIn("[a.b.c]", ctx)
        self.assertEqual(ctx['[a.b.c]'].value, "_:test")

    @unittest.expectedFailure
    def test_generalisation_with_vars(self):
        sen1 = self.dsl("a.b.c(::test.blah)")[0]
        sen2 = self.dsl("a.b.$x(::test.$a)")[0]

        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertIn("[a.b.c]", ctx)
        self.assertEqual(ctx['[a.b.c]'].value, "_:test")


    @unittest.expectedFailure
    def test_specialisation(self):
        sen1 = self.dsl("a.b.c(::test)")[0]
        sen2 = self.dsl("a.b.$x(::test.bloo)")[0]

        ctx = CheckStatementFragment().structural_check([sen1], [sen2], None, ctxs=CtxSet())
        self.assertIn("[a.b.c]", ctx)
        self.assertEqual(ctx['[a.b.c]'].value, "_:test.bloo")


    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[==]": SimpleTypeMatch()})

        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        result = semsys(instr, ctxs=ctxs)


    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[==]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        result = semsys(instr, ctxs=ctxs)

    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[==]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        with self.assertRaises(TE.AcabTypingException):
            semsys(instr, ctxs=ctxs)

    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[==]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        with self.assertRaises(TE.AcabTypingException):
            semsys(instr, ctxs=ctxs)

    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[==]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(== TYPE_DEF)?")[0], ctxs=ctxs)
        semsys(instr, ctxs=ctxs)

    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[τ=]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(::TYPE_DEF)?")[0], ctxs=ctxs)
        semsys(instr, ctxs=ctxs)


    @unittest.expectedFailure
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

        ctxs = CtxSet({"[∈]": ELEM(), "[τ=]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("a.$y(::TYPE_DEF, ∈ def)?")[0], ctxs=ctxs)

        result = semsys(instr, ctxs=ctxs)

    @unittest.expectedFailure
    def test_semantics_operator_matching(self):
        """
        Check
        """
        semsys = DEFAULT_SEMANTICS()
        semsys.register(CheckStatementFragment().build_semantics())
        self.dsl.register(CheckStatementFragment().build_dsl()).build()

        trans_sen = self.dsl("a.test.transform(::χ): \n λan.op.def test.sen val.$var -> $y \nend")[0]
        # trans_sen = self.dsl['transform.core']("λan.op.def test.sen val.$var -> $y")[0]
        type_     = self.dsl("an.op.def(::λ): $x $y $z")[0]
        instr     = self.dsl['transform.statement'].parse_string('⊢ @x')[0]

        semsys(trans_sen)
        semsys(type_)

        ctxs = CtxSet({"[∈]": ELEM(), "[τ=]": SimpleTypeMatch()})


        semsys(self.dsl("a.test.$x?")[0], ctxs=ctxs)
        semsys(self.dsl("an.op.$y?")[0], ctxs=ctxs)
        result = semsys(instr, ctxs=ctxs)
        pass


    @unittest.skip("todo")
    def test_op_type_constraint_check(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y -> $z")[0]

        trans_use = self.dsl("a.test.$x(::blah)?")[0]
        trans_sen = trans_use[1:]

        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())
        self.assertEqual(ctx['[x__type]'], "_:test.sen")
        self.assertEqual(ctx['[y__type]'], "_:val.var")
        self.assertEqual(ctx.y, 'z__type')

    @unittest.skip("todo")
    def test_op_constraint_check(self):
        trans_def = self.dsl['sentence.ends'].parse_string("def(::λ): $x $y -> $z")[0]

        trans_use = self.dsl("a.test.$x(< $y)?")[0]
        breakpoint()
        trans_sen = trans_use[1:]

        ctx = CheckStatementFragment().structural_check([trans_sen], [trans_def[0]], None, ctxs=CtxSet())
        self.assertEqual(ctx['[x__type]'], "_:test.sen")
        self.assertEqual(ctx['[y__type]'], "_:val.var")
        self.assertEqual(ctx.y, 'z__type')


if __name__ == '__main__':
    unittest.main()
