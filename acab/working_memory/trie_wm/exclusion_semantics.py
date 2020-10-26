# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.node_semantics import AcabNodeSemantics

from .util import EXOP as EXOP_enum

from acab.config import AcabConfig

config = AcabConfig.Get()
OPERATOR_S = config("Parsing.Structure", "OPERATOR_S")
EXOP = config("WorkingMemory.TrieWM", "EXOP")
DEFAULT_EXOP = config("WorkingMemory.TrieWM", "DEFAULT_EXOP")

class ExclusionNodeSemantics(AcabNodeSemantics):
    def accessible(self, node, data, term):
        potentials = []
        # Expand if variable -> Grab All
        if term.is_var and term.name not in data:
            potentials += node.children
            # Get only matching child if variable is already set
        elif term.is_var:
            assert(term.name in data)
            value = data[term.name]
            if self.contain(node, value):
                potentials.append(node.get_child(value))

        elif self.contain(node, term):
            potentials.append(node.get_child(term))

        if OPERATOR_S in term._data:
            potentials = [x for x in potentials if x._data[OPERATOR_S] == term._data[OPERATOR_S]]
        return potentials

    def lift(self, word, constructor):
        assert(isinstance(word, AcabValue))
        exop = EXOP_enum[DEFAULT_EXOP]
        if EXOP in word._data:
            exop = word._data[EXOP]
        elif OPERATOR_S in word._data:
            exop = word._data[OPERATOR_S]

        word_node = constructor(word, {OPERATOR_S: exop})
        return word_node


    def contain(self, node, query_term):
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not query_term in node:
            return False

        return True


    def get(self, node, query_term):
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        result = None
        if self.contain(node, query_term):
            result = node.get_child(query_term)

        return result

    def add(self, node, word, node_constructor) -> Tuple[bool, AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(word, AcabValue))
        assert(callable(node_constructor))

        is_new_node = False
        # insert the target and cause changes
        result = self.get(node, word)

        if result is None:
            result = self.lift(word, node_constructor)
            is_new_node = True

        if node._data[OPERATOR_S] is EXOP_enum.EX and \
        len(node.children) >= 1 and result is not None:
            node.clear_children()

        node.add_child(result)

        # coerce to exclusive if necessary
        if OPERATOR_S in word._data and word._data[OPERATOR_S] is EXOP_enum.EX:
            result._data[OPERATOR_S] = EXOP_enum.EX

        return is_new_node, result

    def delete(self, node, to_delete):
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = node.remove_child(to_delete)
        return removed
