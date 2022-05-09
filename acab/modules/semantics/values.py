import logging as logmod
from dataclasses import dataclass
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.error.semantic as ASErr
from acab import types as AT
from acab.interfaces import value as VI
from acab.core.config.config import AcabConfig
from acab.core.value.instruction import Instruction
from acab.core.data.node import AcabNode
from acab.core.value.sentence import Sentence
from acab.core.semantics import basic
from acab.interfaces import semantic as SI
from acab.error.protocol import AcabProtocolError as APE

Node  = AcabNode
T     = TypeVar('T')

config       = AcabConfig()

CTX_OP = Enum("ctx", "collect_var")
# TODO replace operator with specific modal name
EXOP         = config.prepare("MODAL", "exop")()
DEFAULT_EXOP = config.default(EXOP)
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

logging = logmod.getLogger(__name__)

Sentence = AT.Sentence
Node     = AT.Node
Engine   = AT.Engine
Value    = AT.Value
T        = TypeVar('T')

# Independent Semantics
@APE.assert_implements(SI.ValueSemantics_i)
class BasicNodeSemantics(basic.ValueSemantics, SI.ValueSemantics_i):

    def verify(self, instruction) -> bool:
        return isinstance(instruction, VI.Value_i)
    def make(self, val, data=None) -> Node:
        return self.up(AcabNode(val), data=data)

    def up(self, node:Node, *, data=None) -> Node:
        """ The Most Basic Lift, does nothing """
        return node

    def access(self, node:Node, term:Value, *, data=None) -> list[Node]:
        if term is None:
            return list(iter(node))

        potentials = []
        if term is None:
            potentials += node.children.values()
        elif node.has(term):
            potentials.append(node.get(term))

        return potentials

    def insert(self, node:Node, new_node:Node, *, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(new_node, AcabNode))
        if new_node in node:
            raise ASErr.AcabSemanticIndependentFailure("Node is already child", context=(node, new_node))

        key = new_node.value.key()
        return node.add(new_node, key=key)

    def remove(self, node:Node, to_delete:Value, *, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, VI.Value_i))

        if to_delete not in node:
            raise ASErr.AcabSemanticIndependentFailure("Value not in node", context=(node, to_delete))

        return node.remove(to_delete)


    def update(self, node:Node, term:Value, *, data:None|dict[Any, Any]=None) -> None: pass
    def __call__(self, *args:Any, **kwargs:Any) -> Any: pass

@APE.assert_implements(SI.ValueSemantics_i)
class ExclusionNodeSemantics(basic.ValueSemantics, SI.ValueSemantics_i):
    def verify(self, instruction) -> bool:
        return isinstance(instruction, VI.Value_i)

    def make(self, val, *, data=None) -> AcabNode:
        return self.up(AcabNode(val), data=data)

    def up(self, node:Node, *, data=None) -> Node:
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

    def access(self, node:Node, term:Value, *, data=None) -> list[Node]:
        potentials = []

        if term is None:
            potentials += node.children.values()
        elif isinstance(term, VI.Sentence_i) and node.has(str(term)):
            potentials.append(node.get(str(term)))
        elif node.has(term):
            potentials.append(node.get(term))

        # TODO add hook for additional test fragments here?
        # TODO check type of term v potentials

        if bool(term) and EXOP in term.data and any([x.data[EXOP] != term.data[EXOP] for x in potentials]):
            raise ASErr.AcabSemanticIndependentFailure(f"EXOP MisMatch, expected {term.data[EXOP]}",
                                                       context=term)


        return potentials

    def insert(self, node:Node, new_node:Node, *, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(new_node, AcabNode))

        if new_node in node:
            raise ASErr.AcabSemanticIndependentFailure("Node is already child", rest=(node, new_node))

        if node.data[EXOP] is EXOP_enum.EX and len(node.children) >= 1:
            node.clear()

        result = node.add(new_node)
        return result

    def remove(self, node:Node, to_delete:Value, *, data=None) -> Node:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, VI.Value_i))

        if to_delete not in node:
            raise ASErr.AcabSemanticIndependentFailure("Value not in node", rest=(node, to_delete))

        return node.remove(to_delete)

    def update(self, node:Node, term:Value, *, data:None|dict[Any, Any]=None) -> None: pass
    def __call__(self, *args:Any, **kwargs:Any) -> Any: pass
