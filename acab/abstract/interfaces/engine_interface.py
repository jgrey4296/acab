from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import abc
from dataclasses import dataclass, field

from acab.abstract.interfaces.dsl_interface import DSL_Interface, DSLBuilder_Interface
from acab.abstract.interfaces.semantic_interfaces import SemanticSystem
from acab.abstract.interfaces.printing_interfaces import PrintSystem
from acab.abstract.interfaces.module_loader_interface import ModuleLoader_Interface

@dataclass
class AcabEngine_Interface(metaclass=abc.ABCMeta):

    # Root components to extend
    parser         : DSL_Interface          = field()
    semantics      : SemanticSystem         = field()
    printer        : PrintSystem            = field()

    # Modules to load
    modules        : List[str]              = field(default_factory=list)
    # Files to load
    load_paths     : List[str]              = field(default_factory=list)
    init_strs      : List[str]              = field(default_factory=list)

    initialised    : bool                   = field(init=False, default=False)
    # Abstract fields, need to be instantiated:
    _dsl_builder   : DSLBuilder_Interface   = field(init=False)
    _module_loader : ModuleLoader_Interface = field(init=False)

    @abc.abstractmethod
    def load_file(self, filename):
        # TODO doesn't need to be abstract?
        pass

    @abc.abstractmethod
    def save_file(self, filename: str, printer=None):
        # TODO doesn't need to be abstract?
        pass

    @abc.abstractmethod
    def __call__(self, thing, bindings=None):
        pass
