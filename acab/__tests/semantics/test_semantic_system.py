#!/opt/anaconda3/envs/acab/bin/python
import sys
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))
import logging
import unittest
import unittest.mock as mock
from os.path import split, splitext

import acab

config = acab.setup()

import acab.modules.semantics.abstractions as ASem
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.node import AcabNode
from acab.abstract.core.production_abstractions import (ActionOperator,
                                                        ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.interfaces.semantic_interfaces import (AbstractionSemantics,
                                                          SemanticSystem)
from acab.error.acab_base_exception import AcabBaseException
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.operators.query.query_operators import EQ, NEQ, HasTag
from acab.modules.operators.transform.transform_operators import RegexOp
from acab.modules.semantics.context_container import (ConstraintCollection,
                                                      ContextContainer,
                                                      ContextInstance)
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
from acab.modules.semantics.system import BasicSemanticSystem
from acab.modules.semantics.util import SemanticOperatorWrapDecorator

EXOP         = config.value("MODAL", "exop")
EXOP_enum    = config.modal_enums[EXOP]

NEGATION_V   = config.value("Value.Structure", "NEGATION")
BIND_V       = config.value("Value.Structure", "BIND")
CONSTRAINT_V = config.value("Value.Structure", "CONSTRAINT")
QUERY_V      = config.value("Parse.Structure", "QUERY")
TRANSFORM_V  = config.value("Parse.Structure", "TRANSFORM")
ACTION_V     = config.value("Parse.Structure", "ACTION")

SEMANTIC_HINT_V = config.value("Value.Structure", "SEMANTIC_HINT")

QUERY_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "QUERY")])
ACTION_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "ACTION")])
TRANSFORM_SEM_HINT = Sentence.build([config.value("SEMANTICS", "TRANSFORM")])
RULE_SEM_HINT      = Sentence.build([config.value("SEMANTICS", "RULE")])
AGENDA_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "AGENDA")])
LAYER_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "LAYER")])
PIPELINE_SEM_HINT  = Sentence.build([config.value("SEMANTICS", "PIPELINE")])

# TODO test verify

class SemanticSystemTests(unittest.TestCase):

    class StubAbsSemantic(AbstractionSemantics):
        def __call__(self, ins, ctxCon, semSys, data=None):
            raise AcabBaseException("TestAbsSem called")

    def test_construction(self):
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)
        self.assertIsInstance(semsys, SemanticSystem)
        self.assertIsInstance(semsys.base, AbstractionSemantics)
        self.assertFalse(semsys.mapping)
        self.assertFalse(semsys.structs)

    def test_default_call(self):
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)
        test_sen = Sentence.build(["test"])
        with self.assertRaises(AcabBaseException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception._str, "TestAbsSem called")

    def test_retrieval(self):
        # put some semantics in semsys.mapping
        pass

    def test_failure(self):
        # put a callable in failure
        pass

    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
