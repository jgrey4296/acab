"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
import collections.abc as cABC
import logging as root_logger
import traceback
import sys
from types import TracebackType
from dataclasses import dataclass, field
from importlib import import_module
from types import ModuleType
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT
from acab.error.import_exception import AcabImportException

logging = root_logger.getLogger(__name__)

Sentence            = AT.Sentence
Handler_Fragment    = AT.Handler_Fragment
ProductionOperator  = AT.Operator

#----------------------------------------
@dataclass(frozen=True)
class ModuleComponents():
    """ Simple holder for extracted module components """

    source        : str                    = field()
    dsl_fragments : List[Handler_Fragment] = field()
    semantics     : List[Handler_Fragment] = field()
    printers      : List[Handler_Fragment] = field()
    operators     : List[Sentence]         = field()

    def report(self):
        frags     = f"{ len(self.dsl_fragments) } DSL Fragments"
        semantics = f"{ len(self.semantics) } Semantic Components"
        printers  = f"{ len(self.printers) } Printers"
        operators = f"{ len(self.operators) } Operators"

        return f"Module {self.source}:\n- {frags}\n- {semantics}\n- {operators}\n- {printers}"

    def __repr__(self):
        frags     = f"{ len(self.dsl_fragments) } DSL"
        semantics = f"{ len(self.semantics) } Sem"
        printers  = f"{ len(self.printers) } Pr"
        operators = f"{ len(self.operators) } Op"

        return f"({frags} | {semantics} | {operators} | {printers} : {self.source})"


#--------------------
@dataclass
class ModuleLoader_i(cABC.Mapping):
    """ Describes how an engine loads ACAB/py modules """
    loaded_modules       : Dict[str, ModuleComponents]  = field(init=False, default_factory=dict)

    def __getitem__(self, key):
        return self.loaded_modules[key]

    def __repr__(self):
        loaded_modules = ", ".join([x for x in self.loaded_modules.keys()])
        return f"ModuleLoader({loaded_modules})"

    def __iter__(self):
        return self.loaded_modules.values()

    def __len__(self):
        return len(self.loaded_modules)

    def __contains__(self, other: Union[ModuleType, str]):
        return str(other) in self.loaded_modules

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
        """
        # Prepare path
        if not isinstance(maybe_module, (ModuleType, str)):
            raise AcabImportException(f"Bad Module Load Type: {maybe_module}")

        try:
            # print semantics: basic+word join of "."
            # mod_str = str(module_sen)

            # Return early if already loaded
            if str(maybe_module) in self.loaded_modules:
                logging.warning("Module already loaded: {}".format(maybe_module))
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
            raise AcabImportException(f"Module Not Found: {maybe_module}").with_traceback(err.__traceback__) from err
        except NameError as err:
            new_err = AcabImportException(f"{maybe_module} : {err}").with_traceback(err.__traceback__)
            raise new_err from err

    @abc.abstractmethod
    def extract_from_module(self, module: ModuleType) -> ModuleComponents:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        pass

