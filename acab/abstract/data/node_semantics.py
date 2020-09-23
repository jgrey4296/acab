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
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode

class AcabNodeSemantics:
    """ A Single Class to provide
    interchangeable core semantics
    Always handles AcabNodes wrapping AcabValues

    """
    def equal(self, word : AcabNode, word2 : AcabNode) -> bool:
        raise NotImplementedError()

    def lift(self, word : AcabValue) -> AcabNode:
        """ Lifting a value to a data node """
        # could include vocabulary tracking a la spacy
        raise NotImplementedError()


    def add(self, node : AcabNode, to_add : AcabValue) -> AcabNode:
        raise NotImplementedError()

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        """ Getting a node from the data structure """
        raise NotImplementedError()

    def contain(self, node : AcabNode, query_term : AcabValue) -> bool:
        """ Getting Node inclusion in a set """
        raise NotImplementedError()

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        """ Removing a node from the data structure """
        raise NotImplementedError()
