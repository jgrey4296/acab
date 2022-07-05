#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from __future__ import annotations

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
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    from acab.modules.context.constraints import ConstraintCollection
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.core.data.node import AcabNode
    from acab.interfaces.value import ValueFactory as VF
    from acab.modules.operators.query.query_operators import ELEM
    from acab.modules.context.context_set import ContextSet
    import acab.error.semantic as ASErr


class TestConstraintCollection(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.build()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_creation_empty(self):
        """
        CC with a word that has no constraints
        """
        ConstraintCollection.operators = None
        constraints = ConstraintCollection(self.dsl("test")[0][0])
        self.assertFalse(constraints.operators)
        self.assertFalse(constraints._test_mappings)

    def test_creation_non_empty(self):
        ConstraintCollection.operators = None
        constraints = ConstraintCollection(self.dsl("$x(∈ a.b.c)")[0][0])
        self.assertFalse(constraints.operators)
        self.assertIn("alpha", constraints._test_mappings)

    def test_bool_empty(self):
        constraints = ConstraintCollection(self.dsl("test")[0][0])
        self.assertFalse(constraints)


    def test_bool_non_empty(self):
        constraints = ConstraintCollection(self.dsl("$x(∈ a.b.c)")[0][0])
        self.assertTrue(constraints)

    def test_constraint_test_alpha(self):
        constraints = ConstraintCollection(self.dsl("$x(∈ a.b.c)")[0][0],
                                           operators={"∈": ELEM()})
        node = AcabNode(VF.value("b"))
        constraints.test(node, ContextSet()[0])

    def test_constraint_test_fail(self):
        constraints = ConstraintCollection(self.dsl("$x(∈ a.b.c)")[0][0],
                                           operators={"∈": ELEM()})
        node = AcabNode(VF.value("d"))

        with self.assertRaises(ASErr.AcabSemanticTestFailure) as cm:
            constraints.test(node, ContextSet()[0])

        self.assertEqual(cm.exception.detail, "Alphas Failed")
        self.assertEqual(cm.exception.context[0], node)


    def test_constraint_beta(self):
        constraints = ConstraintCollection(self.dsl("$x(∈ $y)")[0][0],
                                           operators={"∈": ELEM()})
        self.assertIn("beta", constraints._test_mappings)

    def test_constraint_test_beta(self):
        constraints = ConstraintCollection(self.dsl("$x(∈ $y)")[0][0],
                                           operators={"∈": ELEM()})
        node = AcabNode(VF.value("c"))
        ctx = ContextSet()[0].progress({"y": self.dsl("a.b.c")[0]}, {})[0]

        constraints.test(node, ctx)

    def test_constraint_test_beta_fail(self):
        constraints = ConstraintCollection(self.dsl("$x(∈ $y)")[0][0],
                                           operators={"∈": ELEM()})
        node = AcabNode(VF.value("c"))
        ctx = ContextSet()[0].progress({"y": self.dsl("d.e.f")[0]}, {})[0]

        with self.assertRaises(ASErr.AcabSemanticException) as cm:
            constraints.test(node, ctx)

        self.assertEqual(cm.exception.detail, "Betas Failed")
        self.assertEqual(cm.exception.context[0], node)


    @unittest.skip("todo")
    def test_constraint_substruct_test(self):
        pass

    @unittest.skip("todo")
    def test_constraint_substract_test_fail(self):
        pass

    @unittest.skip("todo")
    def test_constraint_run_name(self):
        pass

    @unittest.skip("todo")
    def test_constraint_run_name_fail(self):
        pass
