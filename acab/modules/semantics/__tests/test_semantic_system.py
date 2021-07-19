#!/opt/anaconda3/envs/acab/bin/python
import sys
from os.path import abspath, expanduser
import unittest

sys.path.append(abspath(expanduser("~/github/acab")))

import acab

config = acab.setup()

from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.core.values import Sentence
from acab.abstract.interfaces.semantic_interfaces import (AbstractionSemantics,
                                                          SemanticSystem)
from acab.error.acab_base_exception import AcabBaseException
from acab.modules.semantics.context_container import (ContextContainer,
                                                      ContextInstance)
from acab.modules.semantics.independent import ExclusionNodeSemantics
from acab.modules.semantics.system import BasicSemanticSystem

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.prepare("Value.Structure", "NEGATION")()
BIND_V       = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V = config.prepare("Value.Structure", "CONSTRAINT")()
QUERY_V      = config.prepare("Parse.Structure", "QUERY")()
TRANSFORM_V  = config.prepare("Parse.Structure", "TRANSFORM")()
ACTION_V     = config.prepare("Parse.Structure", "ACTION")()

SEMANTIC_HINT_V = config.prepare("Value.Structure", "SEMANTIC_HINT")()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])

# TODO test verify
# semSys      = BasicSemanticSystem(trie_sem.query, trie_struct,
#                                   mapping={QUERY_SEM_HINT  : query_sem,
#                                            ACTION_SEM_HINT : action_sem,
#                                            TRANSFORM_SEM_HINT: trans_sem,
#                                            RULE_SEM_HINT   : rule_sem},
#                                   key=SemHintKey)
# sem_sys = PrintSystem(handlers=[
#     Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
#     Printers.ConstraintAwareValuePrinter("_:ATOM"),
#     Printers.ProductionComponentPrinter("_:COMPONENT"),
#     # Printers.ContainerPrinter("_:CONTAINER"),
#     Printers.StructurePrinter("_:STRUCTURE"),
#     Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
#     Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL"),
#     Printers.StatementPrinter("_:STATEMENT")
# ],
#                       settings={"MODAL": "exop"})




class SemanticSystemTests(unittest.TestCase):

    class StubAbsSemantic(AbstractionSemantics):
        def __call__(self, ins, semSys, ctxs=None, data=None):
            raise AcabBaseException("TestAbsSem called")

    def test_construction(self):
        semsys = BasicSemanticSystem(default=(SemanticSystemTests.StubAbsSemantic("_stub"), None),
                                     handlers=[], structs=[])
        self.assertIsInstance(semsys, SemanticSystem)
        self.assertIsInstance(semsys.default[0], AbstractionSemantics)
        self.assertFalse(semsys.registered_handlers)
        self.assertFalse(semsys.registered_structs)

    def test_default_call(self):
        semsys = BasicSemanticSystem(default=(SemanticSystemTests.StubAbsSemantic("_:stub"), None),
                                     handlers=[], structs=[])
        test_sen = Sentence.build(["test"])
        with self.assertRaises(AcabBaseException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception.detail, "TestAbsSem called")

    def test_retrieval(self):
        # put some semantics in semsys.mapping
        semsys = BasicSemanticSystem(default=(SemanticSystemTests.StubAbsSemantic("_:stub"), None),
                                     handlers=[], structs=[])



    def test_failure(self):
        # put a callable in failure
        pass

    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
