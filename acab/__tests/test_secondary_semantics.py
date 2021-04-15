#!/usr/bin/env python3

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging
from acab.abstract.config.config import AcabConfig
config       = AcabConfig.Get("acab/abstract/config")

from acab.error.acab_semantic_exception import AcabSemanticException
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.production_abstractions import ProductionComponent, ProductionContainer
from acab.modules.semantics.independent import BasicNodeSemantics, ExclusionNodeSemantics
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.context_container import ContextContainer, ContextInstance, ConstraintCollection
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag
from acab.modules.operators.transform.transform_operators import RegexOp

import acab.modules.semantics.abstractions as ASem

EXOP         = config.value("MODAL", "exop")
EXOP_enum    = config.modal_enums[EXOP]

NEGATION_V   = config.value("Value.Structure", "NEGATION")
BIND         = config.value("Value.Structure", "BIND")
CONSTRAINT_V = config.value("Value.Structure", "CONSTRAINT")

class FSMSemanticTests(unittest.TestCase):
    def test_fsm_insert(self):
        pass

    def test_fsm_trigger(self):
        pass



class ASPSemanticTests(unittest.TestCase):
    pass

class SemanticHookTests(unittest.TestCase):
    pass
