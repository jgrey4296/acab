#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, Generic

from uuid import uuid1
import itertools as it
from enum import Enum
import logging as root_logger
from dataclasses import dataclass, field
from fractions import Fraction

from acab.error.acab_semantic_exception import AcabSemanticException
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue, AcabStatement
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode

from acab.abstract.interfaces import semantic_interfaces as SI

from . import util as SemUtil

Value = AcabValue
Node  = AcabNode
T     = TypeVar('T')

config         = AcabConfig.Get()
NEGATION_S   = config.value("Value.Structure", "NEGATION")
CONSTRAINT_S = config.value("Value.Structure", "CONSTRAINT")
AT_BIND_S    = config.value("Value.Structure", "AT_BIND")
BIND       = config.value("Value.Structure", "BIND")

CTX_OP = Enum("ctx", "collapse")
# TODO replace operator with specific modal name
EXOP         = config.value("MODAL", "exop")
DEFAULT_EXOP = config.modal_defaults[EXOP]
EXOP_enum    = config.modal_enums[EXOP]

logging = root_logger.getLogger(__name__)

Sentence = 'Sentence'
Node     = 'Node'
Engine   = 'Engine'

# Independent Semantics
class BasicNodeSemantics(SI.IndependentSemantics):

    def make(self, val):
        raise NotImplementedError()

    def up(self, word: AcabValue, data=None) -> AcabNode:
        """ The Most Basic Lift, does nothing """
        return AcabNode(word)


    def down(self, value):
        raise NotImplementedError()
    def access(self, node, term, data=None):
        potentials = []
        value = None
        # if looking for unbound variable -> Grab All
        if term is None or (term.is_var and term.name not in data):
            potentials += node.children.values()
        # Get only matching child if variable is already set
        elif term.is_var:
            assert(term.name in data)
            value = data[term.name]
        else:
            value = term

        if value and node.has_child(value):
            potentials.append(node.get_child(value))

        return potentials

    def insert(self, node, new_node, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(new_node, AcabNode))
        if new_node in node:
            raise AcabSemanticException("Node is already child", (node, new_node))

        return node.add_child(new_node)

    def remove(self, node, to_delete: AcabValue) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        if to_delete not in node:
            raise AcabSemanticException("Value not in node", (node, to_delete))

        return node.remove_child(to_delete)


    def equal(self, val1, val2) -> bool:
        raise NotImplementedError()

class ExclusionNodeSemantics(SI.IndependentSemantics):
    def make(self, val):
        pass

    def up(self, word: Value, data) -> Node:
        # Add an exop if necessary
        node = AcabNode(word)
        node.data[EXOP] = DEFAULT_EXOP
        if EXOP in word.data:
            node.data[EXOP] = word.data[EXOP]

        return node


    def down(self, node):
        pass
    def access(self, node, data, term):
        potentials = []
        value = None
        # Expand if variable -> Grab All
        if term.is_var and term.name not in data:
            potentials += node.children.values()
        # Get only matching child if variable is already set
        elif term.is_var:
            assert(term.name in data)
            value = data[term.name]

        if self.contain(node, value):
            potentials.append(node.get_child(value))

        if EXOP in term.data:
            potentials = [x for x in potentials if x.data[EXOP] == term.data[EXOP]]

        return potentials

    def insert(self, node, new_node, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(new_node, AcabNode))

        if new_node in node:
            raise AcabSemanticException("Node is already child", (node, new_node))

        if node.data[EXOP] is EXOP_enum.EX and len(node.children) >= 1:
            node.clear_children()

        result = node.add_child(new_node)
        return result

    def remove(self, node, to_delete, data=None):
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        if to_delete not in node:
            raise AcabSemanticException("Value not in node", (node, to_delete))

        return node.remove_child(to_delete)



    def equal(self, word, word2):
        pass

    def _contain(self, node, query_term):
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not query_term in node:
            return False

        return True





# Mixins
class DebugListenerMixin(SI.SemanticMixin):
    """ TODO formerly SubSemantics
    Listeners for triggers
    """
    """ The core used to *debug* WM action, using listeners """
    listeners : Set[Any] = field(init=False, default_factory=set)
    listeners_threshold : Fraction = field(init=False, default=Fraction(1,2))
    # TODO use these to enable breakpoint context:
    _production_stack : List['ProductionAbstraction'] = field(init=False, default_factory=list)

    def clear_listeners(self):
        self._listeners = set()

    def register_listeners(self, words):
        self._listeners.update(words)

    def unregister_listeners(self, words):
        self._listeners.difference_update(words)

    def set_listener_threshold(self, a, b):
        self._listener_threshold = Fraction(a,b)

    def score_listener(self, words):
        simple_words = [str(x) if not x.is_var else "$_" for x in words]
        num_in_listener_bag = sum([1 if x in self._listeners else 0 for x in simple_words])
        sentence_fraction = Fraction(num_in_listener_bag, len(simple_words))
        if sentence_fraction >= self._listener_threshold:
            return True

        return False

    def breakpoint(self):
        # TODO: add more listener options: pre, on and post
        breakpoint()




    def add_listeners(self, *words):
        """ Add basic data breakpoints """
        self._working_memory.register_listeners(words)

    def remove_listeners(self, *words):
        """ Remove data breakpoints """
        self._working_memory.unregister_listeners(words)

    def set_listener_threshold(self, a, b):
        """ Specify the number of word matches
        are needed to trigger the breakpoint """
        self._working_memory.set_listener_threshold(a, b)

    def get_listeners(self):
        return self._working_memory._listeners

    def get_listener_threshold(self):
        return self._working_memory._listener_threshold


class MessageMixin(Generic[T], SI.SemanticMixin):
    """ Inter-Semantic Communication """
    context      : List[T]        = field(init=False, default_factory=list)
    stack        : List[T]        = field(init=False, default_factory=list)
    queue        : List[T]        = field(init=False, default_factory=list)
    accumulation : Dict[str, Any] = field(init=False, default_factory=dict)
    # TODO: defaultdict

    # TODO refine this
    def _add_to_context(self, value):
        if isinstance(value, str):
            self._context.append(value)
        elif isinstance(value, list):
            self._context += value
        else:
            raise Exception("Expected a str or a list")

    def _add_to_accumulation(self, value):
        assert isinstance(value, dict)
        self._accumulation.update(value)

    def _push_stack(self, data, sentinel, params):
        assert isinstance(data, list)
        self._stack.append((self._queue, self._context))

        if sentinel is not None:
            data.append((SemUtil.RET_enum.SENTINEL, data, sentinel, params))

        self._queue = data
        self._context = []

    def _pop_stack(self):
        if not bool(self._queue) and bool(self._stack):
            stack_q, stack_ctx = self._stack.pop()
            self._queue = stack_q
            self._context = stack_ctx


class QuerySemanticMixin(SI.SemanticMixin):
    """ Provides Semantics to test a node

    """
    def test_candidates(self, term, candidate_triple, tests, engine):
        """ query word, with a sequence of tests,
        in all available bind groups, BFS manner
        """
        assert(not term.is_at_var)
        # TODO: validate on activate context too

        # Get the frontier
        potentials = [(i, d, passing) for i,d,n in candidate_triple
                      for passing in self.accessible(n, d, term)]

        passing = potentials

        # Filter the frontier
        if tests is not None:
            alphas, betas, subbinds, annotations, variable_ops = tests

            if bool(variable_ops):
                raise Exception("TODO: Variable Ops")

            # Apply Tests
            # TODO: test type instance if not ATOM
            passing = [self._run_subbinds(n, subbinds, d, i, engine=engine) for i,d,n in potentials
                       if self._test_alphas(n, alphas, d, engine=engine)
                       and self._test_betas(n, betas, d, engine=engine)
                       and self._test_annotations(n, annotations, d, engine=engine)]


        to_add = [(i, self._prepare_dict(d, n, term), n) for i,d,n in passing if n is not None]
        return to_add


    # internal
    def _run_subbinds(self, node, subbind_comps, data, i, engine=None):
        # TODO Factor this into queryop subbind?
        invalidated = False
        new_data = {}
        result = None
        for subbind in subbind_comps:
            # TODO: expand subbind usng data first
            result = SemUtil._compare(subbind, node, data=data, engine=engine)
            if result is None:
                invalidated = True
                break

            assert(isinstance(result, dict))
            for k, v in result.items():
                if k not in data:
                    new_data[k] = v
                elif data[k] != v:
                    invalidated = True
                    break

        if invalidated:
            node = None
            final_data = None
        else:
            final_data = data.copy()
            final_data.update(new_data)

        return (i, final_data, node)

    def _test_alphas(self, node, comps, data=None, engine=None):
        """ Run alpha tests against word retrieved value """
        return all([SemUtil._compare(word, node, data=data, engine=engine) for word in comps])

    def _test_betas(self, node, comps, data=None, engine=None):
        """ Run word beta tests against word retrieved value, with supplied bindings """
        return all([SemUtil._compare(word, node, data=data, engine=engine) for word in comps])

    def _test_annotations(self, node, comps, data=None, engine=None):
        return all([SemUtil._compare(word, node, data=data, engine=engine) for word in comps])

    def _prepare_dict(self, data: dict, passing_node: AcabNode, query_term: AcabValue):
        """ Prepare a tuple for adding to the context

        """
        data_copy = {}
        data_copy.update(data)
        if query_term.is_var:
            data_copy[query_term.name] = passing_node.value
            data_copy[AT_BIND_S + query_term.name] = passing_node

        return data_copy

# Handlers
class QueryHandlers(SI.SemanticHandler):
    """ Utility sub semantic behaviour """

    def _start_word_semantics(self, structure, contexts, clause):
        binding = None
        if clause[0].is_at_var:
            binding = clause[0].value

        contexts.force_node_position(target=structure.root, binding=binding)

        if binding is not None:
            return clause[1:]

        return clause

    def _collapse_semantics(self, ctxs, collapse_set):
        if bool(collapse_set):
            ctxs.collapse(collapse_set)

    def _negation_semantics(self, contexts, clause):
        if NEGATION_S in clause.data and clause.data[NEGATION_S]:
            contexts.invert()

    def _clause_query(self, structure, clause: Sentence, contexts, engine):
        """ Test a single clause,
        annotating contexts upon success and failure """
        logging.debug("Testing Clause: {}".format(repr(clause)))

        clause = self._start_word_semantics(structure, contexts, clause)
        # go down from the root by query element:
        # For each word of the clause sentence, eg: a. in a.b.word
        collapse_on = set()
        for word in clause:
            tests, annotations = self._validate_and_split_constraints(word, ctx=contexts, engine=engine)
            # This is hardcoded currently
            if CTX_OP.collapse in annotations and word.is_var:
                collapse_on.add(word.name)

            logging.debug("Testing node: {}".format(repr(word)))
            logging.debug("Current Contexts: {}".format(len(contexts)))
            node_groups, ancestor_tracker = contexts.group_by_type()
            # Pair each context triple with a semantics to use
            group_semantics = {}
            for x,y in node_groups.items():
                sem = self.retrieve_semantics(x)
                if sem not in group_semantics:
                    group_semantics[sem] = []
                group_semantics[sem] += y

            # test each active alternative
            passing_candidates = [
                r
                for sem, triples in group_semantics.items()
                for r in sem.test_candidates(word, triples, tests, engine)
            ]

            # Merge then add
            contexts.clear()
            contexts.append(passing_candidates, ancestor_tracker)

            # TODO add in context growth restrictions?
            if not bool(contexts):
                break

        self._collapse_semantics(contexts, collapse_on)
        self._negation_semantics(contexts, clause)
        self._failure_semantics(contexts, clause)

        if not bool(contexts):
            raise AcabSemanticException("No successful contexts", str(clause))

        return contexts

    def _validate_and_split_constraints(self, word):
        """ Split tests into (alphas, betas, sub_binds),
        Also connect Components to actual operators
        """
        if CONSTRAINT_S not in word.data:
            return (None, set())

        constraints = word.data[CONSTRAINT_S]
        annotations = set()
        callable_annotations = []
        alphas = []
        betas = []
        sub_binds = []
        variable_ops = []
        for c in constraints:
            if not isinstance(c, ProductionComponent) and hasattr(c, "__call__"):
                callable_annotations.append(c)
            # intentionally not elif:
            if not isinstance(c, ProductionComponent):
                annotations.add(c)
            # intentionally elifs:
            elif c.is_var:
                variable_ops.append(c)
            elif c.is_sub_bind_test:
                sub_binds.append(c)
            elif c.is_alpha_test:
                alphas.append(c)
            else:
                betas.append(c)

        return (
            (alphas, betas, sub_binds, callable_annotations, variable_ops),
            annotations,
        )



    def _get(self, structure, sentence):
        assert isinstance(structure, AcabStruct)
        assert isinstance(sentence, Sentence)

        # Get the root
        current = structure.root
        # Get Nodes
        for word in sentence:
            mapping = self.retrieve_semantics(type(current))
            current = mapping.get(current, word)
            if current is None:
                return []

        return [current]
