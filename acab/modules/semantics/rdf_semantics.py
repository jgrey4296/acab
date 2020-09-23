# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.contexts import Contexts
from acab.abstract.data.structure import DataStructure
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.config import AcabConfig

util = AcabConfig.Get()

NEGATION_S = util("Parsing.Structure", "NEGATION_S")

class RDFSemantics(AcabStructureSemantics):
    """
    Semantics for Subject Predicate Object Triples
    """
    def __init__(self, node_semantics, node_type,
                 leaf_type=None, update_data=None, update_func=None):
        super(BasicTrieSemantics, self).__init__(node_semantics)

        self._node_type = node_type
        self._leaf_type = leaf_type or node_type
        self._update_data = {}
        self._update_func = update_func or (lambda a,b,c: a)

        if update_data is not None:
            self._update_data.update(update_data)

    def add(self, structure, to_add, leaf_data=None):
        assert(isinstance(structure, Trie))
        assert(isinstance(to_add, Sentence))

        # Get the root
        current = structure.root
        current_path = []
        # Add to nodes
        for word in to_add:
            current = self._ns.add(current, word)
            self._update_func(current, curr_path, self._update_data)
            current_path.append(current)

            # TODO Register new nodes with structure weak index

        return current

    def get(self, structure, sentence):
        pass

    def contain(self, structure, sentence):
        pass

    def delete(self, structure, sentence):
        pass

    def query(self, structure, clause, ctxs, engine):
        pass

    def down(self, data_structure):
        pass

    def lift(self, sentences):
        pass
