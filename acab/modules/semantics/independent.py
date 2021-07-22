import logging as root_logger
from enum import Enum
from typing import TypeVar

import acab.error.acab_semantic_exception as ASErr
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.interfaces import semantic as SI
from dataclasses import dataclass

Value = AcabValue
Node  = AcabNode
T     = TypeVar('T')

config       = AcabConfig.Get()
NEGATION_S   = config.prepare("Value.Structure", "NEGATION")()
CONSTRAINT_S = config.prepare("Value.Structure", "CONSTRAINT")()
AT_BIND_S    = config.prepare("Value.Structure", "AT_BIND")()
BIND         = config.prepare("Value.Structure", "BIND")()

CTX_OP = Enum("ctx", "collapse")
# TODO replace operator with specific modal name
EXOP         = config.prepare("MODAL", "exop")()
DEFAULT_EXOP = config.default(EXOP)
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

logging = root_logger.getLogger(__name__)

Sentence = 'Sentence'
Node     = 'Node'
Engine   = 'Engine'

# Independent Semantics
class BasicNodeSemantics(SI.IndependentSemantics_i):

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
            raise ASErr.AcabSemanticIndependentFailure("Node is already child", (node, new_node))

        return node.add_child(new_node)

    def remove(self, node, to_delete: AcabValue, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        if to_delete not in node:
            raise ASErr.AcabSemanticIndependentFailure("Value not in node", (node, to_delete))

        return node.remove_child(to_delete)

class ExclusionNodeSemantics(SI.IndependentSemantics_i):
    def make(self, val, data=None) -> AcabNode:
        return self.up(AcabNode(val), data)

    def up(self, node: Node, data=None) -> Node:
        # Add an exop if necessary
        if EXOP in node.data:
            return node

        node.data[EXOP] = DEFAULT_EXOP
        word = node.value
        if EXOP in word.data:
            node.data[EXOP] = word.data[EXOP]

        if bool(data) and EXOP in data:
            node.data[EXOP] = data[EXOP]

        return node

    def access(self, node, term, data=None, get_all=False):
        potentials = []
        value = None
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
            raise ASErr.AcabSemanticIndependentFailure("Node is already child", (node, new_node))

        if node.data[EXOP] is EXOP_enum.EX and len(node.children) >= 1:
            node.clear_children()

        result = node.add_child(new_node)
        return result

    def remove(self, node, to_delete, data=None):
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        if to_delete not in node:
            raise ASErr.AcabSemanticIndependentFailure("Value not in node", (node, to_delete))

        return node.remove_child(to_delete)

