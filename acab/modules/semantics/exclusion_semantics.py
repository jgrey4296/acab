# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.abstract.config.config import AcabConfig
from acab.abstract.config.modal import MODAL_DEFAULTS, MODAL_ENUMS

config = AcabConfig.Get()
# TODO replace operator with specific modal name
EXOP = config.value("MODAL", "exop")
DEFAULT_EXOP = MODAL_DEFAULTS[EXOP]
EXOP_enum = MODAL_ENUMS[EXOP]


# Fixup the last modal operator if a sentence has been inserted
# TODO move this to exclusion semantics
# if isinstance(retrieved, Sentence):
#     output += [y.copy() for y in retrieved]
#     output[-1]._data[OPERATOR] = word._data[OPERATOR]
#     continue

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

        if EXOP in term._data:
            potentials = [x for x in potentials if x._data[EXOP] == term._data[EXOP]]
        return potentials

    def lift(self, word, constructor):
        assert(isinstance(word, AcabValue))
        exop_val = DEFAULT_EXOP
        if EXOP in word._data:
            exop_val = word._data[EXOP]

        word_node = constructor(word, {EXOP: exop_val})
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

        if node._data[EXOP] is EXOP_enum.EX and \
        len(node.children) >= 1 and result is not None:
            node.clear_children()

        node.add_child(result)

        # coerce to exclusive if necessary
        if EXOP in word._data and word._data[EXOP] is EXOP_enum.EX:
            result._data[EXOP] = EXOP_enum.EX

        return is_new_node, result

    def delete(self, node, to_delete):
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = node.remove_child(to_delete)
        return removed
