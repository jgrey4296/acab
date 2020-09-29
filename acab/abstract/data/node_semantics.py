"""
Semantics Stub Class

Working Memory / Layer takes, uses it throughout

default rule semantics
Typing semantics
exclusion semantics
etc

Semantics fall into a hierarchy:
Structure : open/closed world,
 exclusion, modal, binding, context growth
 reinforcement, matching
↓
Node : lift, contains, call
↓
Value : transform, call, query


"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.rule.query import QueryComponent

from acab.config import AcabConfig

util = AcabConfig.Get()
NEGATION_S = util("Parsing.Structure", "NEGATION_S")
CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")
AT_BIND_S = util("Parsing.Structure", "AT_BIND_S")

class AcabNodeSemantics:
    """ A Single Class to provide
    interchangeable core semantics
    Always handles AcabNodes wrapping AcabValues

    """
    def accessible(self, word : AcabNode, term : AcabValue) -> [AcabNode]:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        raise NotImplementedError()

    def equal(self, word : AcabNode, word2 : AcabNode) -> bool:
        raise NotImplementedError()

    def lift(self, word : AcabValue) -> AcabNode:
        """ Lifting a value to a data node """
        # could include vocabulary tracking a la spacy
        raise NotImplementedError()


    def add(self, node : AcabNode, to_add : AcabValue) -> AcabNode:
        raise NotImplementedError()

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        """ Getting a node from the data structure """
        raise NotImplementedError()

    def contain(self, node : AcabNode, query_term : AcabValue) -> bool:
        """ Getting Node inclusion in a set """
        raise NotImplementedError()

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        """ Removing a node from the data structure """
        raise NotImplementedError()




    def _test_word(self, query_term, query_context):
        """ query word, with a sequence of tests,
        in all available bind groups, BFS manner
        """
        assert(not query_term.is_at_var)
        engine = query_context._engine
        # TODO: validate on activate context too
        alphas, betas, subbinds, annotations = self.validate_and_split_constraints(query_term, engine=engine)
        callable_annotations = [word for word in annotations if hasattr(word, "__call__")]
        data_set = {word : d for word,d in enumerate(query_context.pairs())}
        pairs  = list(enumerate(query_context.pairs()))
        query_context.clear()


        potentials = [(i, d[0], passing) for i,d in pairs for passing in self.accessible(d, query_term)]
        # Apply Tests
        # TODO: test type instance if not ATOM
        passing = [self.run_subbinds(n, subbinds, d, i, engine=engine) for i,d,n in potentials
                   if self.test_alphas(n, alphas, d, engine=engine)
                   and self.test_betas(n, betas, d, engine=engine)
                   and self.test_annotations(n, callable_annotations, d, engine)]

        to_add = [query_context.prepare_tuple_dict(i, d, n, query_term) for i,d,n in passing if n is not None]

        query_context.append(*to_add, fail_dict=data_set)
        return annotations


    def validate_and_split_constraints(self, word, ctx=None, engine=None):
        """ Split tests into (alphas, betas, sub_binds),
        Also connect Components to actual operators
        """
        # TODO make this a node semantic
        if CONSTRAINT_S not in word._data:
            return ([], [], [], set())

        comps = [word.verify(ctx=ctx, engine=engine) for word in word._data[CONSTRAINT_S] if isinstance(word, QueryComponent)]
        others = set([word for word in word._data[CONSTRAINT_S] if not isinstance(word, QueryComponent)])
        alphas = []
        betas = []
        sub_binds = []
        for c in comps:
            if c.is_sub_bind_test:
                sub_binds.append(c)
            elif c.is_alpha_test:
                alphas.append(c)
            else:
                betas.append(c)

        return (alphas, betas, sub_binds, others)

    def run_subbinds(self, node, subbind_comps, data, i, engine=None):
        # TODO Factor this into queryop subbind?
        invalidated = False
        new_data = {}
        result = None
        for subbind in subbind_comps:
            # TODO: expand subbind usng data first
            result = subbind(node, data=data, engine=engine)
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

    def test_alphas(self, node, comps, data=None, engine=None):
        """ Run alpha tests against word retrieved value """
        return all([word(node, data=data, engine=engine) for word in comps])

    def test_betas(self, node, comps, data=None, engine=None):
        """ Run word beta tests against word retrieved value, with supplied bindings """
        return all([word(node, data=data, engine=engine) for word in comps])

    def test_annotations(self, node, comps, data=None, engine=None):
        return all([word(node, data=data, engine=engine) for word in comps])
