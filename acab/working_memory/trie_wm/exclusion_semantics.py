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

    def lift(self, word):
        assert(isinstance(word, AcabValue))
        exop = EXOP_enum[DEFAULT_EXOP]
        if EXOP in word._data:
            exop = word._data[EXOP]

        return AcabNode(word, data={OPERATOR_S: exop})


    def contain(self, node, query_term):
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not query_term.name in node._children:
            return False

        child_exop = node._children[query_term.name]._data[OPERATOR_S]
        query_exop = query_term._data[OPERATOR_S]
        operator_matches = query_exop == child_exop

        return operator_matches

    def get(self, node, query_term):
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if self.contain(node, query_term):
            return node._children[query_term.name]

        return None

    def add(self, node, to_add):
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_add, AcabValue))

        # insert the target and cause changes
        if self.contain(node, to_add):
            return self.get(node, to_add)

        if to_add._data[OPERATOR_S] is EXOP_enum.EX:
            node.clear_children()
            

        new_node = self.lift(to_add)
        node._children[to_add.name] = new_node

        return new_node

    def delete(self, node, to_delete):
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = self.get(node, to_delete)
        if removed is not None:
            del node._children[to_delete.name]

        return removed
