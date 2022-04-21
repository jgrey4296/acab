#!/usr/bin/env python3
import logging as logmod

import acab.interfaces.semantic as SI
import acab.error.semantic as ASErr
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence

logging = logmod.getLogger(__name__)
config = AcabConfig()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()
QUERY_FALLBACK_S = config.prepare("Value.Structure", "QUERY_FALLBACK")()
DEFAULT_SETUP_S  = config.prepare("Data", "DEFAULT_SETUP_METHOD")()
DEFAULT_UPDATE_S = config.prepare("Data", "DEFAULT_UPDATE_METHOD")()

Node          = 'AcabNode'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'


class FSMSemantics(SI.StructureSemantics_i):

    def insert(self, struct, sen, data=None, ctxs=None):
        """
        In the FSM, everything is access from the root,
        there is a defined start node,
        sentences define sequences of connections

        eg:
        a.b.c.a : defines a circular path

        a.b.d   : defines a split
        a.c.d

        -a.b.c  : disconnects a from b, and b from c
        """
        if data is None:
            data = {}

        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            return self._delete(struct, sen, data, engine)

        # Get the root
        root      = self.default[0].up(struct.root)
        current   = root
        root_spec = self.lookup(root.value)
        for word in sen:
            new_node = None
            root_accessible = root_spec[0].access(root, word, data)
            if not bool(root_accessible):
                next_spec = self.lookup(word)
                new_node = next_spec[0].make(word, data)
                root_spec[0].insert(root, word, data)
            else:
                new_node = root_accessible[0]

            current_spec       = self.lookup(current)
            current_accessible = current_spec[0].access(current, new_node, data)
            if not bool(current_accessible):
                current = current_spec[0].insert(current, new_node, data)
            else:
                current = new_node

        return current

    def query(self, struct, query, data=None, ctxs=None):
        """
        Test that all states exist, and connections line up:
        a.b.c.d?

        Or that connections are *not* there:
        ~a.b.c?

        Use a node test to check state?:
        a(λactive).b.c.d?
        a.b(λactive).c.d? etc

        Or Get the active state:
        $x(λactive)?

        Test for loops:
        a.b.a?

        Test for available movements:
        $x(λactive).$y?
        """
        # Query from start to finish
        if ctxs is None:
            raise ASErr.AcabSemanticException("Ctxs is None to fsm query", query)

        negated_query = False
        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            negated_query = True

        # TODO get collapse vars from the sentence
        collapse_vars = []
        with ctxs(struct.root, sen[0], data, collapse_vars, negated_query):
            for word in sen:
                for ctxInst in ctxs.active_list():
                    spec = self.lookup(ctxInst.current)
                    results = spec[0].access(ctxInst.current, word, data)
                    if not bool(results):
                        ctxs.fail(ctxInst, word, None)
                    else:
                        ctxs.test(word, ctxInst, results)

        return ctxs


    def _delete(self, struct, sen, data=None, ctxs=None):
        """
        remove each word in the sentence from its prior
        """
        root = struct.root
        root_spec = self.lookup(root.value)
        current = None
        for head,succ in zip(sen[:-1], sen[1:]):
            if root_spec[0].access(root, head, data):
                head_node = root_sem.get(root, head, data)
                head_spec = self.lookup(head_node.value)
                head_spec[0].delete(head_node, succ, data)
                current = head_node

        return current

    def trigger(self, struct, sen, data=None, ctxs=None):
        """
        Trigger the FSM.
        ie:
        [empty sentence] : Use any available transition
        a.b              : Use the defined transition
        """
        raise NotImplementedError()
