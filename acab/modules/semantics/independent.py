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

    def make(self, val, data=None) -> AcabNode:
        return self.up(AcabNode(val), data)

    def up(self, node: AcabNode, data=None) -> AcabNode:
        """ The Most Basic Lift, does nothing """
        return node

    def access(self, node, term, data=None, get_all=False):
        # TODO possible shift get_all into data
        potentials = []
        if get_all:
            potentials += node.children.values()
        elif node.has_child(term):
            potentials.append(node.get_child(term))

        return potentials

    def insert(self, node, new_node, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(new_node, AcabNode))
        if new_node in node:
            raise AcabSemanticException("Node is already child", (node, new_node))

        return node.add_child(new_node)

    def remove(self, node, to_delete: AcabValue, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        if to_delete not in node:
            raise AcabSemanticException("Value not in node", (node, to_delete))

        return node.remove_child(to_delete)

    def equal(self, val1, val2) -> bool:
        raise NotImplementedError()

class ExclusionNodeSemantics(SI.IndependentSemantics):
    def make(self, val, data=None) -> AcabNode:
        return self.up(AcabNode(val), data)

    def up(self, node: Node, data=None) -> Node:
        # Add an exop if necessary
        if EXOP in node.data:
            return node

        word = node.value
        node.data[EXOP] = DEFAULT_EXOP
        if EXOP in word.data:
            node.data[EXOP] = word.data[EXOP]

        return node

    def access(self, node, term, data=None, get_all=False):
        potentials = []
        value = None
        # Expand if variable -> Grab All
        if get_all:
            potentials += node.children.values()
        elif node.has_child(term):
            potentials.append(node.get_child(term))

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
