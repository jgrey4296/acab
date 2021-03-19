#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from uuid import uuid1, UUID

CtxIns = 'ContextInstance'

@dataclass
class ContextContainer():

    # is operators a static var?
    operators      : Dict[str, Callable] = field(default_factory=dict)
    ctxs           : List[CtxIns]        = field(default_factory=list)
    purgatory      : List[CtxIns]        = field(default_factory=list)
    failed         : List[CtxIns]        = field(default_factory=list)
    _negated       : Bool                = field(default=False)
    _collapse_vars : Set[str]            = field(default_factory=set)

    def active(self):
        # return a copied list of ctxs
        pass

    def __enter__(self, root_node, root_word, data, collapse_vars, is_negated=False):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        self._negated = is_negated
        self._collapse_vars.update(collapse_vars)
        if root_word.is_at_var:
            [inst.set_current_node(root_node) for x in self.ctxs]
        else:
            bound_ctxs = []
            for inst in self.ctxs:
                try:
                    bound_ctxs.append(inst.set_current_binding(root_word))
                except AcabSemanticException:
                    logging.info("Ctx lacks binding")
                self.ctxs = bound_ctxs


    def __exit__(self):
        # Handle instances in purgatory
        # collapse bindings as necessary
        self._collapse_on()


    def fail(self, instance: CtxIns):
        # move the given instance to purgatory
        pass

    def test(self, word: 'AcabValue', ctx: CtxIns, possible: List['AcabNode']):
        # TODO
        # run a word's tests on available nodes, with an instance

        # get tests and annotations

        # run bind test
        # run alpha tests
        # run beta tests

        # if success, bind

        # else self.fail on the context

        pass

    def _collapse_on(self):
        """
        Context collapse on specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collapse:
        1[ctx]n -> 1[c:ctx]1
        where
        c = a_ctx = { on_var: [x[on_var] for x in ctxs] }
        """
        if not bool(self._collapse_vars):
            return

        # select instances with bindings
        # Merge into single new instance
        # replace



    def _filter_candidates(self, target_pattern, candidates, match_func):
        """ Filter candidates using match_func to compare
        against this data_structure

        Where a Match = [(PatternNode, MatchNode)]
        Return [Match]

        match_func : Node -> [Node] -> [Node]
        """
        # TODO check this
        assert(isinstance(target_pattern, AcabStruct))

        if isinstance(candidates, AcabStruct):
            candidates = candidates.root

        if not isinstance(candidates, AcabNode):
            raise AcabBaseException()

        final_matches = []
        pattern_nodes = list(candidates.children.values())
        # (current pattern position, available choices, match state)
        queue = [(word, pattern_nodes, []) for word in target_pattern.root.children.values()]

        while bool(queue):
            current, available_nodes, match_state = queue.pop(0)

            matching_nodes = match_func(current, available_nodes)
            for node in matching_nodes:
                next_match_state = match_state + [(current, node)]

                if bool(current):
                    next_available = list(node.children.values())
                    next_patterns = list(current.children.values())
                    queue += [
                        (word, next_available, next_match_state)
                        for word in next_patterns
                    ]
                else:
                    final_matches.append(next_match_state)

        return final_matches






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

@dataclass
class ContextInstance():

    uuid         : UUID                      = field(default_factory=uuid1)
    current      : 'AcabNode'                = field()
    data         : Dict[Any, Any]            = field(default_factory=dict)
    nodes        : Dict[Any, 'AcabNode']     = field(default_factory=dict)
    continuation : 'AcabStatement'           = field(init=False, default=None)

    def bind(self, word, nodes):
        # create len(nodes) of new instances, each with the word's binding added
        pass

    def set_current_node(self, node):
        self.current = node

    def set_current_binding(self, word):
        if word.name not in self.nodes:
            raise AcabSemanticException("No Recognised binding")

        self.current = self.nodes[word.name]
        return self

    def merge_with(self, var, instance_list):
        # TODO
        merged_instance = ContextInstance()


        return merged_instance

    def continue(self):
        # Run the continuation (eg: run the transform and action of a rule)
        pass

# Required Utilities
# DEPRECATED: use ContextContainer/ ContextInstance instead
@dataclass
class ContextTracker(ContextInterface):
    """
    Container of available contexts for word match in the trie
    Conceptually a list of tuples: ({}, LastAccessedNode)
    And Stores failure state
    """

    def group_by_type(self) -> Tuple[Dict['type', List[Tuple[int, 'dict', 'node']]],
                                     Dict[int, 'tuple']]:
        """
        group contexts together by the type of the node it is at
        """
        pairs = self.pairs()
        ancestor_tracker = {}
        groups = {}
        for i, xy in enumerate(pairs):
            the_type = type(xy[1])
            if the_type not in groups:
                groups[the_type] = []
            groups[the_type].append((i, xy[0], xy[1]))
            ancestor_tracker[i] = xy
        return groups, ancestor_tracker

    def _param_ctx_filter(self, params, ctxs):
        """ Return only contexts which have all the needed parameters """
        # verify params -> ctxs fit
        result = []
        param_set = params
        for ctx_singular in ctxs:
            ctx_set = ctx_singular
            if param_set in ctx_set:
                result.append(ctx_singular)

        return result


class QuerySemanticComponent():
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
