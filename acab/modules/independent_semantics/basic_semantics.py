# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
import logging as root_logger

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue
from acab.abstract.core.contexts import Contexts, CTX_OP
from acab.abstract.core.node import AcabNode
from acab.abstract.core.node_semantics import AcabNodeSemantics
from acab.abstract.containers.struct_semantics import AcabStructureSemantics
from acab.abstract.containers.structure import DataStructure

from acab.abstract.interfaces import semantics_interface as SI

import acab.abstract.containers.struct_semantics as SSem

from acab.abstract.config.config import AcabConfig

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()


class BasicNodeSemantics(AcabNodeSemantics, SI.NodeSemantics):

    # TODO Needs equal method
    def accessible(self, node, data, term):
        potentials = []
        # Expand if variable -> Grab All
        if term.is_var and term.name not in data:
            potentials += node.children.values()
        # Get only matching child if variable is already set
        elif term.is_var:
            assert(term.name in data)
            value = data[term.name]
            if self.contain(node, value):
                potentials.append(node.get_child(value))

        elif self.contain(node, term):
            potentials.append(node.get_child(term))

        return potentials

    def up(self, word: AcabNode) -> AcabNode:
        """ The Most Basic Lift, does nothing """
        return word


    def contain(self, node: AcabNode, query_term: AcabValue) -> bool:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))
        in_children = query_term in node
        return in_children

    def get(self, node: AcabNode, query_term: AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not self.contain(node, query_term):
            return None

        return node.get_child(query_term.name)

    def add(self, node: AcabNode, word: AcabValue, node_constructor: Callable) -> Tuple[bool, AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(word, AcabValue))

        is_new_node = False
        result = self.get(node, word)

        if result is None:
            new_node = node_constructor(word)
            result = self.up(new_node)
            node.add_child(result)
            is_new_node = True

        return is_new_node, result

    def delete(self, node: AcabNode, to_delete: AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = node.remove_child(to_delete)
        return removed


    def down(self, value):
        raise NotImplementedError()
    def equal(self, val1, val2) -> bool:
        raise NotImplementedError()
