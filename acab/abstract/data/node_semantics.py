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
from typing import Callable, Iterator, Union, Match, AliasType
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.trie import Trie

class AcabNodeSemantics:
    """ A Single Class to provide
    interchangeable core semantics
    Always handles AcabNodes wrapping AcabValues

    """
    def equal(self, word : AcabNode, word : AcabNode) -> Bool:
        raise NotImplementedError()

    def lift(self, word : AcabValue) - > AcabNode:
        """ Lifting a value to a data node """
        # could include vocabulary tracking a la spacy
        raise NotImplementedError()


    def add(self, node : AcabNode, to_add : AcabValue) -> AcabNode:
        raise NotImplementedError()

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        """ Getting a node from the data structure """
        raise NotImplementedError()

    def contain(self, node : AcabNode, query_term : AcabValue) -> Bool:
        """ Getting Node inclusion in a set """
        raise NotImplementedError()

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        """ Removing a node from the data structure """
        raise NotImplementedError()


class BasicNodeSemantics(AcabNodeSemantics):

    def lift(self, word : AcabValue) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return AcabNode(word)


    def contain(self, node : AcabNode, query_term : AcabValue) -> Bool:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))
        in_children = query_term.name in node._children
        return in_children

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not self.contain(node, query_term):
            return None

        return node._children[query_term.name]

    def add(self, node : AcabNode, to_add : AcabValue) -> AcabNode:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_add, AcabValue))

        if self.contain(node, to_add):
            return self.get(node, to_add)

        new_node = self.lift(to_add)
        node._children[to_add.name] = new_node

        return new_node

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = self.get(node, to_delete)
        if removed is not None:
            del node._children[to_delete.name]

        return removed


class ExclusionNodeSemantics(AcabNodeSemantics):

    def lift(self, word):
        assert(isinstance(word, AcabValue))
        exop = DOT
        if "EXOP" in word._data:
            exop = word._data[EXOP]

        return AcabNode(word, data={EXOP: exop})


    def contain(self, node, query_term):
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not query_term.name in node._children:
            return False

        child_exop = node._children[query_term.name]._data[exop]
        operator_matches = query_term.exop == child_exop

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
            return self.get(to_add)

        if to_add.exop is EX:
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


class TypeNodeSemantics(AcabNodeSemantics):
    def lift(self, word : AcabValue) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return AcabNode(word)


    def contain(self, node : AcabNode, query_term : AcabValue) -> Bool:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))
        in_children = query_term.name in node._children
        return in_children

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not self.contain(node, query_term):
            return None

        return node._children[query_term.name]

    def add(self, node : AcabNode, to_add : AcabValue) -> AcabNode:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_add, AcabValue))

        if self.contain(node, to_add):
            return self.get(node, to_add)

        new_node = self.lift(to_add)
        node._children[to_add.name] = new_node

        return new_node

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = self.get(node, to_delete)
        if removed is not None:
            del node._children[to_delete.name]

        return removed



