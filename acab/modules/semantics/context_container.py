#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
logging = root_logger.getLogger(__name__)

from dataclasses import InitVar, dataclass, field, replace
from uuid import uuid1, UUID

from acab.abstract.config import GET
from acab.abstract.core.production_abstractions import ProductionComponent

config = GET()
CONSTRAINT_S = config.value("Parse.Structure", "CONSTRAINT")


CtxIns      = 'ContextInstance'
Constraints = 'ConstraintCollection'
ProdComp    = 'ProductionComponent'
Operator    = 'ProductionOperator'
Value       = 'AcabValue'
Statement   = 'AcabStatement'
Sentence    = 'Sentence'
Node        = 'AcabNode'

@dataclass
class ConstraintCollection():
    """ Simple container of all ProductionComponent constraints a word possesses,
    separated into subtypes """

    _alphas      : List[ProdComp]      = field(default_factory=list)
    _betas       : List[ProdComp]      = field(default_factory=list)
    _bind        : Value               = field(default=None)
    _annotations : List[ProdComp]      = field(default_factory=list)
    _callables   : List[ProdComp]      = field(default_factory=list)
    _variables   : List[Value]         = field(default_factory=list)
    _operators   : CtxIns              = field(default=None)

    @staticmethod
    def build(word, operators) -> Constraints:
        """ Split tests into (alphas, betas, sub_binds),
        Also connect Components to actual _operators
        """
        bind                 = None
        if word.is_var:
            bind = word

        if CONSTRAINT_S not in word.data:
            return ConstraintCollection(_bind=bind)

        constraints          = word.data[CONSTRAINT_S]
        annotations          = set()
        callable_annotations = []
        alphas               = []
        betas                = []
        variable_ops         = []


        for c in constraints:
            if not isinstance(c, ProductionComponent) and hasattr(c, "__call__"):
                callable_annotations.append(c)
            # intentionally not elif:
            if not isinstance(c, ProductionComponent):
                annotations.add(c)
            # intentionally elifs:
            elif c.is_var:
                variable_ops.append(c)
            elif any([p.is_var for p in c.params]):
                betas.append(c)
            else:
                alphas.append(c)

        return ConstraintCollection(alphas,
                                    betas,
                                    bind,
                                    annotations,
                                    callable_annotations,
                                    variable_ops,
                                    operators)


    def alphas(self, node):
        """ Run alpha tests on a node """
        # Get the (operator, params, data) trio:
        test_trios = [(self._operators[x.op],
                       x.params,
                       x.data) for x in self._alphas]
        # Perform the tests:
        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise AcabSemanticException()

    def betas(self, node, ctxInst):
        """ Run Beta Tests on a node and context isntance """
        test_trios = []
        for test in self._betas:
            op     = self._operators[test.op]
            params = [ctxInst[x] for x in test.params]
            trio   = (op, params, test.data)
            test_trios.append(trio)

        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise AcabSemanticException()

    def binds(self, node, ctxInst):
        """ Check node against prior binding """
        # TODO use independent semantic equality?
        if self._bind is None:
            return
        if self._bind not in ctxInst:
            return

        if node.value != ctxInst[self._bind]:
            raise AcabSemanticException()

    def callable_annotations(self, node, ctxInst):
        test_trios = [(x.op, ctxInst.get(x.params), x.data) for x in self._callables]
        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise AcabSemanticException()

    def extend(self, node, ctxInst) -> CtxIns:
        if self._bind is None:
            return ctxInst

        return ctxInst.bind(self._bind, node)


@dataclass
class ContextContainer():

    # Operators could be a pair: (semantics, struct) to query
    # TODO operators is just the results of a prior query
    _operators     : CtxIns              = field(default=None)
    # TODO should this be a dict:?
    _ctxs          : List[CtxIns]        = field(default_factory=list)

    _purgatory     : List[CtxIns]        = field(init=False, default_factory=list)
    _failed        : List[CtxIns]        = field(init=False, default_factory=list)
    _total         : Set[CtxIns]         = field(init=False, default_factory=set)

    _negated       : bool                = field(init=False, default=False)
    _query_clause  : Sentence            = field(init=False, default=None)
    _root_node     : Node                = field(init=False, default=None)
    _collapse_vars : Set[str]            = field(init=False, default_factory=set)

    @staticmethod
    def build(ops:CtxIns=None):
        """ Create the empty context instance """
        return ContextContainer(_operators=ops,
                                _ctxs=[ContextInstance()])

    def active(self):
        """ Get a copy of the active contexts,
        so ctxs can be modified as semantics go
        """
        self._total.update(self._ctxs)
        active = self._ctxs
        self._ctxs = []
        return active


    def __call__(self, root_node, query_sen, data, collapse_vars, is_negated=False):
        """ Prefaces __enter__, storing the relevant values """
        self._negated = is_negated
        self._collapse_vars.update(collapse_vars)
        self._query_clause = query_sen
        self._root_node = root_node
        return self

    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        root_word = self._query_clause[0]
        if root_word.is_at_var:
            self._ctxs = [x.set_current_binding(root_word) for x in self.active()]
        else:
            self._ctxs = [x.set_current_node(self._root_node) for x in self.active()]

        return self


    def __exit__(self, exc_type, exc_value, traceback):
        # TODO Handle instances in _purgatory
        # collapse bindings as necessary
        self._collapse()
        self._negated       = False
        self._collapse_vars = set()
        self._root_node     = None

        # TODO handle exception

    def fail(self, instance: CtxIns, word: Value):
        """ Record a failure, the query sentence that failed,
        and the word that it failed on """
        # add failure details to the instance, of word and query clause

        # remove from _ctxs

        # add to _purgatory
        breakpoint()

        instance._failure_word = word
        self._purgatory.append(instance)

    def test(self, ctx: CtxIns, possible: List[Node], word: Value):
        """
        run a word's tests on available nodes, with an instance
        """
        constraints : ConstraintCollection = ConstraintCollection.build(word, self._operators)
        assert(len(possible) == 1 or constraints._bind)

        for node in possible:
            try:
                # run alpha tests
                constraints.alphas(node)
                # run beta tests
                constraints.betas(node, ctx)
                # run bind test
                constraints.binds(node, ctx)
                # success, so copy and extend ctx instance
                maybe_new_instance = constraints.extend(node, ctx)
                # Add to contextcontainer
                maybe_new_instance.set_current_node(node)
                self._ctxs.append(maybe_new_instance)

            except AcabSemanticException as err:
                logging.exception("Test _failed")
                self.fail(ctx, word)



    def _collapse(self):
        """
        Context collapse on specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collapse:
        1[ctx]n -> 1[c:ctx]1
        where
        c = a_ctx = { on_var: [x[on_var] for x in _ctxs] }
        """
        if not bool(self._collapse_vars):
            return

        # select instances with bindings
        # Merge into single new instance
        # replace
        pass

    def __len__(self):
        return len(self._ctxs)

    def __getitem__(self, index):
        return self._ctxs[index]
@dataclass
class ContextInstance():

    data         : Dict[Any, Any]  = field(default_factory=dict)
    nodes        : Dict[Any, Node] = field(default_factory=dict)
    uuid         : UUID            = field(default_factory=uuid1)

    _continuation     : Statement = field(init=False, default=None)
    _current          : Node      = field(init=False, default=None)
    _failure_sentence : Sentence  = field(init=False, default=None)
    _failure_word     : Value     = field(init=False, default=None)

    def __hash__(self):
        return hash(self.uuid)

    def copy(self):
        return replace(self,
                       uuid=uuid1(),
                       data=self.data.copy(),
                       nodes=self.nodes.copy()
                       )

    def bind(self, word, node) -> CtxIns:
        extension = self.copy()
        assert(self.uuid != extension.uuid)
        assert(id(self.data) != id(extension.data))
        extension.data[str(word)] = node.value
        extension.nodes[str(word)] = node
        return extension

    def set_current_node(self, node):
        self._current = node
        return self

    def set_current_binding(self, word):
        if word.name not in self.nodes:
            raise AcabSemanticException("No Recognised binding")

        self.set_current_node(self.nodes[word.name])
        return self

    def merge_with(self, var, instance_list):
        # TODO
        merged_instance = ContextInstance()


        return merged_instance

    def continuation(self):
        # Run the continuation (eg: run the transform and action of a rule)
        pass

    def get_params(self, params: List[Value]):
        """ Retrieve a value's parameters from a context dict """
        raise NotImplementedError()
        output = []
        # TODO: enable currying?
        for x in params:
            if isinstance(x, Sentence):
                output.append(x.bind(bound_context))
            elif isinstance(x, list):
                output.append([y.bind(bound_context) for y in x])
            elif isinstance(x, AcabValue) and x.is_var:
                assert(x.value in bound_context)
                if x.is_at_var:
                    output.append(bound_context[AT_BIND_S + x.value])
                elif isinstance(bound_context[x.value], list):
                    # TODO does this need to unwrap all list values?
                    output.append(bound_context[x.value])
                else:
                    output.append(bound_context[x.value].value)
            else:
                output.append(x.value)
        return output


    def __contains__(self, value: Value):
        return str(value) in self.data
    def __getitem__(self, value: Value):
        # TODO handle Value *and* sentence
        if value in self:
            return self.data[str(value)]
        else:
            return value
