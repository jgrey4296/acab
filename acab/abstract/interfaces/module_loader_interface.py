"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.engine.util import ModuleComponents

ModuleType   = 'Module'
Sentence     = 'Sentence'

#--------------------
@dataclass
class ModuleLoader_Interface(metaclass=abc.ABCMeta):
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

    def load_module(self, module_str: str) -> ModuleComponents:
        """
        Load a module, extract operators and dsl fragments from it,
        put the operators into the operators store,
        register the dsl fragments for later use

        Returns a working_memory query result of the module
        """
        # Prepare path
        # TODO use utility constants for joining and query
        if not isinstance(module_str, str):
            breakpoint()
            raise Exception("TODO: handle sentence -> str")
        # print semantics: basic+word join of "."
        # mod_str = str(module_sen)

        # Return early if already loaded
        if module_str in self.loaded_modules:
            logging.info("Module already loaded: {}".format(module_str))
            # TODO extract node from return context?
            return self.loaded_modules[module_str]

        # Load
        try:
            the_module = import_module(module_str)
            # Extract
            components = self.extract_from_module(the_module)
            self.loaded_modules[module_str] = components
            return the_module

        except ModuleNotFoundError as e:
            raise AcabImportException(module_str) from None

    @abc.abstractmethod
    def extract_from_module(self, module: ModuleType) -> ModuleComponents:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        pass
