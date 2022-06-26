#!/usr/bin/env python
from __future__ import annotations
from typing import Tuple, Any
from typing import Callable, Iterator, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic, TypeAlias
from typing import TYPE_CHECKING, Protocol, TypeGuard
from typing import Final, final, overload, Protocol, runtime_checkable
from types import ModuleType
from importlib import import_module
import logging as logmod
logging = logmod.getLogger(__name__)

from acab.interfaces.module_loader import ModuleLoader_i
from acab.error.importer import AcabImportException
from acab import types as AT

ModuleFragment = AT.ModuleFragment

if TYPE_CHECKING:
    # tc only imports
    pass

class ModuleLoaderImpl(ModuleLoader_i):
    """
    An implemented Module loader, just lacking `extract_from_module`
    """
    def __getitem__(self, key:str) -> ModuleFragment:
        return self.loaded_modules[key]

    def __repr__(self) -> str:
        loaded_modules = ", ".join(list(self.loaded_modules.keys()))
        return f"<ModuleLoader({loaded_modules})>"

    def __iter__(self) -> Iterator[ModuleFragment]:
        return iter(self.loaded)

    def __len__(self) -> int:
        return len(self.loaded_modules)

    def __contains__(self, other:str) -> bool:
        return str(other) in self.loaded_modules

    def reload_all_modules(self) -> None:
        loaded = list(self.loaded_modules.keys())
        self.loaded_modules.clear()
        self.load(*loaded)

    def load(self, *modules: ModuleType|str) -> list[ModuleFragment]:
        """ Given ModuleInterface objects,
        store them then tell the working memory to load them
        return a list of dictionaries
        """
        loaded = []
        for mod in modules:
            loaded += self._load_module(mod)

        return loaded

    def _load_module(self, maybe_module: ModuleType | str) -> [ModuleFragment]:
        """
        Load a module, extract operators and dsl fragments from it,
        put the operators into the operators store,
        register the dsl fragments for later use
        """
        #pylint: disable=too-many-branches
        # Prepare path
        if not isinstance(maybe_module, (ModuleType, str)):
            raise AcabImportException(f"Bad Module Load Type: {maybe_module}")

        try:
            # print semantics: basic+word join of "."
            # mod_str = str(module_sen)

            # Return early if already loaded
            if str(maybe_module) in self.loaded_modules:
                logging.warning("Module already loaded: {}", maybe_module)
                return self.loaded_modules[str(maybe_module)]

            if isinstance(maybe_module, str):
                the_module = import_module(maybe_module)
            else:
                the_module = maybe_module
            # Extract
            fragments = self.extract_from_module(the_module)
            self.loaded_modules[str(maybe_module)] = fragments

            return fragments

        except ModuleNotFoundError as err:
            raise AcabImportException(f"Module Not Found: {maybe_module}").with_traceback(err.__traceback__) from err
        except NameError as err:
            new_err = AcabImportException(f"{maybe_module} : {err}").with_traceback(err.__traceback__)
            raise new_err from err


    @property
    def loaded(self) -> list[ModuleFragment]:
        return [y for x in self.loaded_modules.values() for y in x]