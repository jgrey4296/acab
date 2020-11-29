"""
Semantics Stub Class

Working Memory / Layer takes, uses it throughout

default rule semantics
Typing semantics
exclusion semantics
etc

Semantics fall into a hierarchy:
Structure: open/closed world,
 exclusion, modal, binding, context growth
 reinforcement, matching
↓
Node: lift, contains, call
↓
Value: transform, call, query


"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.values import AcabValue, AcabStatement
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode

from acab.abstract.interfaces import semantics_interface as SI
from acab.abstract.config.config import AcabConfig

util         = AcabConfig.Get()
NEGATION_S   = util.value("Value.Structure", "NEGATION")
CONSTRAINT_S = util.value("Value.Structure", "CONSTRAINT")
AT_BIND_S    = util.value("Value.Structure", "AT_BIND")

class AcabNodeSemantics(AcabStatement, SI.NodeSemantics):
    """ A Single Class to provide
    interchangeable core semantics
    Always handles AcabNodes wrapping AcabValues

    """
    def __init__(self):
        super(AcabNodeSemantics, self).__init__(name=self.__class__.__name__)

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
            result = _compare(subbind, node, data=data, engine=engine)
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
        return all([_compare(word, node, data=data, engine=engine) for word in comps])

    def _test_betas(self, node, comps, data=None, engine=None):
        """ Run word beta tests against word retrieved value, with supplied bindings """
        return all([_compare(word, node, data=data, engine=engine) for word in comps])

    def _test_annotations(self, node, comps, data=None, engine=None):
        return all([_compare(word, node, data=data, engine=engine) for word in comps])

    def _prepare_dict(self, data: dict, passing_node: AcabNode, query_term: AcabValue):
        """ Prepare a tuple for adding to the context

        """
        data_copy = {}
        data_copy.update(data)
        if query_term.is_var:
            data_copy[query_term.name] = passing_node.value
            data_copy[AT_BIND_S + query_term.name] = passing_node

        return data_copy




def _compare(word, node,  ctxs=None, engine=None):
    # Get op from engine
    op = engine.get_operator(query_component.op)
    # AcabNode -> AcabValue -> Actual Value
    node_value = node.value.value
    params = ProdSem.get_params(data)

    return op(node_value, *params, data=data)
