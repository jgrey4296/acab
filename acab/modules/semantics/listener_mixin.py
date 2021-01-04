# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field
import acab.abstract.interfaces.semantic_interfaces as SI

Sentence = 'Sentence'

ListenerForm = Union[SI.SemanticHandler, Callable]

@dataclass
class ListenerMixin(SI.SemanticMixin):
    """ Maps aribitrary strings to semantics
    rather than mapping values or nodes


    """

    handlers : Dict[str, ListenerForm] = field(default_factory=dict)

    def call(self, instruction: Sentence) -> Optional[ListenerForm]:
        """
        Take an instruction and return a suitable handler
        """
        pass
