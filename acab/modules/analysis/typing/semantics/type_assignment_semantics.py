#!/usr/bin/env python3
import logging as root_logger
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue
from acab.abstract.data.node_semantics import AcabNodeSemantics
from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing import util

logging = root_logger.getLogger(__name__)

class TypingAssignmentSemantics(AcabNodeSemantics):

    def __init__(self):
        pass
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


    def add(self, node : AcabNode, to_add : AcabValue, node_constructor : Callable) -> AcabNode:
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

    def test_word(self, query_term, query_context):
        """ query word, with a sequence of tests,
        in all available bind groups, BFS manner
        """
        assert(not query_term.is_at_var)
        engine = query_context._engine
        # TODO: validate on activate context too
        alphas, betas, subbinds, annotations = self._validate_and_split_constraints(query_term, engine=engine)
        callable_annotations = [word for word in annotations if hasattr(word, "__call__")]
        data_set = {word : d for word,d in enumerate(query_context.pairs())}
        pairs  = list(enumerate(query_context.pairs()))
        query_context.clear()

        potentials = [(i, d[0], passing) for i,d in pairs for passing in self.accessible(d, query_term)]
        # Apply Tests
        # TODO: test type instance if not ATOM
        passing = [self._run_subbinds(n, subbinds, d, i, engine=engine) for i,d,n in potentials
                   if self._test_alphas(n, alphas, d, engine=engine)
                   and self._test_betas(n, betas, d, engine=engine)
                   and self._test_annotations(n, callable_annotations, d, engine)]

        to_add = [query_context.prepare_tuple_dict(i, d, n, query_term) for i,d,n in passing if n is not None]

        query_context.append(*to_add, fail_dict=data_set)
        return annotations



    # internal
class MonoTypedNode(AcabNode):
    """ Base Node for a Type Trie """

    def __init__(self, value, _type=None):
        assert(_type is None or isisntance(_type, TypeInstance))
        super().__init__(value)
        self._type_instance = _type or ATOM
        self._var_node = None

    @property
    def is_var(self):
        return self.value.is_var
    @property
    def type_instance(self):
        return self._type_instance


    def apply_type_instance(self, _type):
        self._type_instance = _type

    def unify_types(self, _type, lookup=None):
        assert(_type is None or isinstance(_type, TypeInstance))

        if self.type_instance == _type:
            return None

        # TODO: unify type instance and type vars separately?

        if self._type_instance < _type:
            self.apply_type_instance(_type)
        elif not _type < self._type_instance:
            raise te.TypeConflictException(_type.pprint(),
                                           self.type_instance.pprint(),
                                           self.name)

        if self._var_node is not None:
            self._var_node.apply_type_instance(self.type_instance)

        return self



# TODO convert to node semantics
# TODO convert to node semantics
class TypeAssignmentTrieNode(MonoTypedNode):
    """ A Node in the Type Assignment Trie.
    Used in type inference.
    Enables linking with variable type trie """

    def __init__(self, value, _type=None, var_node=None):
        assert(var_node is None or isinstance(var_node, MonoTypedNode))
        super().__init__(value, _type=_type)
        self._var_node = var_node

    def update(self, word, lookup=None):
        """ Post-addition update method.
        links self to a lookup-trie word if self is a variable """
        logging.debug("Node: {} updating with {}".format(self._value,
                                                         str(word)))
        # apply type if necessary
        self.unify_types(word.type, lookup=lookup)

        if not (self.is_var == word.is_var):
            # complain if var status doesn't match
            raise te.TypeVariableConflictException(self)

        if self.is_var and self._var_node is None and lookup is not None:
            # if var, connect to var type trie
            self._var_node = lookup.add([self._value], [])
            self._var_node.add_node(self)

    def clear_var_node(self):
        self._var_node = None
