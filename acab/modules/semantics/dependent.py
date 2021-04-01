#!/usr/bin/env python3
import logging as root_logger

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue, AcabStatement
from acab.abstract.core.node import AcabNode
from acab.abstract.core.acab_struct import AcabStruct
from acab.abstract.core.production_abstractions import ProductionContainer

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.error.acab_base_exception import AcabBaseException

from acab.abstract.config.config import AcabConfig
import acab.abstract.interfaces.semantic_interfaces as SI
from acab.abstract.interfaces.data_interfaces import StructureInterface

from acab.modules.structures.trie.util import split_clauses

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

CONSTRAINT_S = config.value("Value.Structure", "CONSTRAINT")
NEGATION_S = config.value("Value.Structure", "NEGATION")
QUERY_FALLBACK_S = config.value("Value.Structure", "QUERY_FALLBACK")
DEFAULT_SETUP_S = config.value("Data", "DEFAULT_SETUP_METHOD")
DEFAULT_UPDATE_S = config.value("Data", "DEFAULT_UPDATE_METHOD")

T = TypeVar('T')
T2 = TypeVar('T2')

Node          = 'AcabNode'
Sentence      = 'Sentence'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'
SemanticUnion = Union['IndependentSemantics', 'DependentSemantics']


# Dependent Semantics
class BasicTrieSemantics(SI.DependentSemantics):
    """
    Trie Semantics which map values -> Nodes
    """

    def _insert(self, struct, sen, data=None):
        if data is None:
            data = {}

        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            return self._delete(struct, sen, data)

        # Get the root
        # TODO: Ensure the struct is appropriate
        current = self.base.up(struct.root)
        for word in sen:
            semantics = self.retrieve(current)
            accessible = semantics.access(current, word, data)
            if bool(accessible):
                current = accessible[0]
            else:
                next_semantics = self.retrieve(word)
                new_node = next_semantics.make(word, data)
                current = semantics.insert(current, new_node, data)

        return current


    def _delete(self, struct, sen, data=None):
        parent = struct.root
        current = struct.root

        for word in sen:
            # Get independent semantics for current
            semantics = self.retrieve(current)
            accessed = semantics.access(current, word, data)
            if bool(accessed):
                parent = current
                current = accessed[0]
            else:
                return None

        # At leaf:
        # remove current from parent
        semantics = self.retrieve(parent)
        semantics.remove(parent, current.value, data)

        return current


    def _query(self, struct, sen, data=None, ctxs=None):
        # Query from start to finish
        if ctxs is None:
            raise AcabSemanticException()

        negated_query = False
        if NEGATION_S in sen.data and sen.data[NEGATION_S]:
            negated_query = True

        # TODO get collapse vars
        collapse_vars = []
        with ctxs(struct.root, sen[0], data, collapse_vars, negated_query):
            for word in sen:
                for ctx in ctxs.active():
                    indep = self.retrieve(ctx.current)
                    results = indep.access(ctx.current, word, data)
                    if not bool(results):
                        ctxs.fail(ctx)
                    else:
                        ctxs.test(word, ctx, results)

        return ctxs


    def _trigger(self, struct, sen, data):
        pass


class FSMSemantics(SI.DependentSemantics):

    def _insert(self, struct, sen, data=None):
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
        root = self.base.up(struct.root)
        current = root
        root_semantics = self.retrieve(root.value)
        for word in sen:
            new_node = None
            root_accessible = root_semantics.access(root, word, data)
            if not bool(root_accessible):
                next_semantics = self.retrieve(word)
                new_node = next_semantics.make(word, data)
                root_semantics.insert(root, word, data)
            else:
                new_node = root_accessible[0]

            current_semantics = self.retrieve(current)
            current_accessible = current_semantics.access(current, new_node, data)
            if not bool(current_accessible):
                current = current_semantics.insert(current, new_node, data)
            else:
                current = new_node

        return current

    def _query(self, struct, query, data=None, ctxs=None):
        """
        Test that all states exist, and connections line up:
        a.b.c.d?

        Or that connections are *not* there:
        ~a.b.c?

        Use a node test to check state?:
        a(λactive).b.c.d?

        Or Get the active state:
        $x(λactive)?

        """
        if ctxs is None:
            self.run_handler("results.init",
                             data=data,
                             engine=engine,
                             start_node=structure.root)

        pos, neg = query.clauses

        for clause in pos:
            self._test_clause(clause)

        with self.run_handler("results.ctx_invert"):
            for clause in neg:
                self._test_clause(clause)

        return self.run_handler("results.finish")


    def _delete(self, struct, sen, data=None):
        """
        remove each word in the sentence from its prior
        """
        root = struct.root
        root_sem = self.retrieve(root.value)
        current = None
        for head,succ in zip(sen[:-1], sen[1:]):
            if root_sem.access(root, head, data):
                head_node = root_sem.get(root, head, data)
                head_sem = self.retrieve(head_node.value)
                head_sem.delete(head_node, succ, data)
                current = head_node

        return current

    def _trigger(self, sen, data, engine):
        """
        Trigger the FSM.
        ie:
        [empty sentence] : Use any available transition
        a.b              : Use the defined transition
        """
        raise NotImplementedError()


class ASPSemantics(SI.DependentSemantics):
    """
    Stub for passing assertions and queries into an ASP program
    """

    def _insert(self, struct, sen):
        """
        construct the ASP program
        """
        pass


    def _query(self, struct, query):
        """
        pass the cached asp program to a solver,
        retrieve results, extract what is needed,
        and return as sentences
        """
        pass
