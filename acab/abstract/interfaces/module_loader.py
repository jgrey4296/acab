"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from importlib import import_module
from types import ModuleType
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.error.acab_import_exception import AcabImportException

Sentence            = 'Sentence'
DSL_Fragment_i      = 'DSL_Fragment_i'
Semantic_Fragment   = 'Semantic_Fragment'
ProductionOperator  = 'ProductionOperator'
PrintSemantics_i    = 'PrintSemantics_i'

#----------------------------------------
@dataclass
class ModuleComponents():
    """ Simple holder for extracted module components """

    dsl_fragments : List[DSL_Fragment_i]    = field()
    semantics     : List[Semantic_Fragment] = field()
    printers      : List[PrintSemantics_i]  = field()
    operators     : List[Sentence]          = field()
#--------------------
@dataclass
class ModuleLoader_i(metaclass=abc.ABCMeta):
    """ Describes how an engine loads ACAB/py modules """
    loaded_modules       : Dict[str, ModuleComponents]  = field(init=False, default_factory=dict)

    def reload_all_modules(self):
        loaded = list(self.loaded_modules.keys())
        self.loaded_modules.clear()
        self._load_modules(loaded)

    def load_modules(self, *modules: List[str]) -> List[ModuleComponents]:
        """ Given ModuleInterface objects,
        store them then tell the working memory to load them
        return a list of dictionaries
        """
        return [self.load_module(x) for x in modules]

    def load_module(self, maybe_module: Union[ModuleType, str]) -> ModuleComponents:
        """
        Load a module, extract operators and dsl fragments from it,
        put the operators into the operators store,
        register the dsl fragments for later use

        Returns a working_memory query result of the module
        """
        # Prepare path
        # TODO use utility constants for joining and query
        if not isinstance(maybe_module, (ModuleType, str)):
            raise AcabImportException(f"Bad Module Load Type: {maybe_module}")

        try:
            # print semantics: basic+word join of "."
            # mod_str = str(module_sen)

            # Return early if already loaded
            if str(maybe_module) in self.loaded_modules:
                logging.info("Module already loaded: {}".format(maybe_module))
                return self.loaded_modules[maybe_module]

            if isinstance(maybe_module, str):
                the_module = import_module(maybe_module)
            else:
                the_module = maybe_module
            # Extract
            components = self.extract_from_module(the_module)
            self.loaded_modules[maybe_module] = components
            return components

        except ModuleNotFoundError as err:
            raise AcabImportException(f"Error attempting to import: {maybe_module}: {err}") from None

    def __contains__(self, other: Union[ModuleType, str]):
        return str(other) in self.loaded_modules

    @abc.abstractmethod
    def extract_from_module(self, module: ModuleType) -> ModuleComponents:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        pass


