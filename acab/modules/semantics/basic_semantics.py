# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
import logging as root_logger

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue
from acab.abstract.data.contexts import Contexts, CTX_OP
from acab.abstract.data.node import AcabNode
from acab.abstract.data.node_semantics import AcabNodeSemantics
from acab.abstract.data.struct_semantics import AcabStructureSemantics
from acab.abstract.data.structure import DataStructure

import acab.abstract.data.struct_semantics as SSem

from acab.config import AcabConfig

logging = root_logger.getLogger(__name__)
util = AcabConfig.Get()

NEGATION_S = util("Parsing.Structure", "NEGATION_S")
FALLBACK_S = util("Parsing.Structure", "FALLBACK_S")

class BasicNodeSemantics(AcabNodeSemantics):

    def accessible(self, word, data, term):
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

        return potentials

    def lift(self, word : AcabValue, constructor : Callable) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return constructor(word)


    def contain(self, node : AcabNode, query_term : AcabValue) -> bool:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))
        in_children = query_term in node
        return in_children

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not self.contain(node, query_term):
            return None

        return node.get_child(query_term)

    def add(self, node : AcabNode, word: AcabValue, node_constructor : Callable) -> AcabNode:
        assert(isinstance(node, AcabNode))
        assert(isinstance(word, AcabValue))

        if self.contain(node, word):
            return self.get(node, word)

        new_node = self.lift(word, node_constructor)
        node.add_child(new_node)

        return new_node

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = node.remove_child(to_delete)
        return removed





