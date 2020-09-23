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

class TypeNodeSemantics(AcabNodeSemantics):
    def lift(self, word : AcabValue) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return AcabNode(word)


    def contain(self, node : AcabNode, query_term : AcabValue) -> bool:
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


class TypingSemantics(AcabStructureSemantics):

    def __init__(self, node_semantics):
        assert(isinstance(node_semantics, AcabNodeSemantics))
        self._ns = node_semantics


    def add(self, structure : DataStructure, to_add : List[Sentence]) -> List[AcabNode]:
        """ Inserting a coherent set of sentences into the structure """
        raise NotImplementedError()

    def get(self, structure : DataStructure, sentence) -> List[AcabNode]:
        """ Getting a path of nodes corresponding to the sentence """
        raise NotImplementedError()

    def contain(self, structure, sentence) -> bool:
        """ Can the sentence be found in the structure """
        raise NotImplementedError()

    def delete(self, structure, sentence) -> List[AcabNode]:
        """ Remove a sentence from the structure """
        raise NotImplementedError()


    def query(self, structure, clause : Sentence, ctxs : Contexts, engine : 'Engine'):
        """ Answer a clause asked of the data structure """
        # TODO is this part of call semantics?
        # open / closed world
        # depth / breath search
        # match as pattern?
        # return type
        pass


    def down(self, data_structure : DataStructure) -> List[Sentence]:
        """ Take a complex data structure down to basic sentences """
        raise NotImplementedError()

    def lift(self, sentences : List[Sentence]) -> DataStructure:
        """ Raise a set of sentences into a data structure """
        raise NotImplementedError()
