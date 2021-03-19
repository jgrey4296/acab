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





