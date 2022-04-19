#!/usr/bin/env python3

import acab

config = acab.GET()

import acab.error.semantic as ASErr
import acab.interfaces.semantic as SI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.data.value import AcabValue
from acab.core.decorators.util import HandleSignal
from acab.interfaces.semantic import StatementSemantics_i
from acab.interfaces.value import Sentence_i
from acab.modules.analysis.type_cqm import TypeContextQueryManager

NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()

CtxSet        = AT.CtxSet
Node          = AT.Node
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
Contexts      = AT.CtxSet

@HandleSignal("_:TYPE_CHECK")
class TypeSemantics(StatementSemantics_i):
    """
    Abstract Semantics for typechecking

    """

    def __call__(self, instruction, semSys, ctxs=None, data=None) -> CtxSet:



        pass


    def query(self, typedef, struct, data=None, ctxs=None):
        """ Breadth First Search Query """
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is none to TrieSemantics.query", sen)

        assert(struct is None)

        with TypeContextQueryManager(typedef, ctxs) as cqm:
            for source_word in cqm.query:
                for bound_word, ctxInst, current_node in cqm.active:
                    spec    = self.lookup(current_node)
                    results = spec[0].access(current_node,
                                             bound_word,
                                             data)

                    cqm.mabye_test(results)
                    # if source_word.type != ATOM:
                    # build_named_set with an update instruction




        return ctxs
