"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
import logging as logmod
import re
from os.path import split
from dataclasses import dataclass, field
from types import ModuleType
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
import importlib

logging = logmod.getLogger(__name__)

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    # tc only imports
    pass

from acab import AcabConfig
from acab.core.engine.util import (applicable, ensure_handler, needs_init,
                                   prep_op_path)
from acab.core.util.part_implementations.module_loader import ModuleLoaderImpl
from acab.core.value.instruction import ActionOperator, ProductionOperator
from acab.interfaces import fragments as FI
from acab.interfaces.value import ValueFactory as VF
from acab.interfaces.value import Sentence_i
from acab.interfaces.fragments import ModuleFragment
from acab.interfaces.module_loader import ModuleLoader_i
from acab.error.importer import AcabImportException

config           = AcabConfig()
MODULE_SPLIT_REG = re.compile(config.attr.Parse.Patterns.MODULE_SPLIT_REG)
MODULE_TARGET    = config.attr.Imports.MODULE_TARGET

#--------------------
class ModuleLoader(ModuleLoaderImpl, ModuleLoader_i):
    """ Describes how an engine loads ACAB/py modules """

    def extract_from_module(self, module: ModuleType) -> list[ModuleFragment]:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        logging.debug("Extracting module: {}", module)
        base_path      = module.__package__
        # reference_path = MODULE_SPLIT_REG.split(module.__name__)
        queue            = [(base_path, module)]
        module_fragments = []
        fragments        = []

        while bool(queue):
            curr_path, curr_mod = queue.pop(0)

            # First, try to get a 'module' file if it exists
            try:
                package_file = split(curr_mod.__name__)[1]
                if package_file != MODULE_TARGET and curr_mod.__name__ == curr_mod.__package__:
                    mod = importlib.import_module(f"{curr_path}.{MODULE_TARGET}")
                    logging.debug(f"Found a `module` file for: {curr_path}")
                    queue += [(mod.__package__, mod)]
                    continue
            except ModuleNotFoundError:
                pass

            # Now get contents
            if hasattr(curr_mod, "__all__"):
                mod_contents = [(x, getattr(curr_mod, x)) for x in getattr(curr_mod, "__all__")]
            else:
                mod_contents = [(x, getattr(curr_mod, x)) for x in dir(curr_mod) if "__" not in x]

            # queue submodules
            queue     += self.handle_submodules(mod_contents, base_path)
            # Then extract fragments
            fragments += self.handle_specific_fragments(mod_contents)
            # extract loose operators
            fragments += self.handle_loose_operators(mod_contents, base_path)
            # And unified fragments
            fragments += self.handle_unified_fragments(mod_contents)
            # And finally module fragments
            module_fragments += self.handle_module_fragments(mod_contents)

        # End of while
        if not (bool(fragments) or bool(module_fragments)):
            raise AcabImportException(f"No Applicable Fragments found in {base_path}")

        # Assemble a module fragment from all loose ones
        result = ModuleFragment(base_path, fragments)

        module_fragments.append(result)
        logging.debug(f"Extracted {len(module_fragments)}: {module_fragments}")

        return module_fragments

    def handle_submodules(self, mod_contents, base):
        sub_modules = [(y.__package__, y) for x,y in mod_contents if isinstance(y, ModuleType)]
        to_queue    = [(x,y) for x,y in sub_modules if base in y.__package__ and "__init__" in y.__file__]
        return to_queue

    def handle_unified_fragments(self, mod_contents):
        extensions = [y for f,y in mod_contents if applicable(y, FI.UnifiedFragment_p)]
        instances  = [y() if needs_init(y) else y for y in extensions]
        return instances

    def handle_specific_fragments(self, mod_contents):
        # Get module dsl_fragments
        available_frags = [y for x,y in mod_contents if applicable(y, FI.HandlerFragment_i)]
        instances       = []
        try:
            for frag in available_frags:
                    match frag:
                        case type():
                            instance = frag()
                            if bool(instance):
                                instances.append(instance)
                        case _ if bool(frag):
                            instances.append(frag)
        except Exception as err:
            raise AcabImportException("Failure Building Fragment", rest=[frag])

        return instances

    def handle_module_fragments(self, mod_contents):
        available_frags =  [y for x,y in mod_contents if isinstance(y, FI.ModuleFragment) and bool(y)]
        return available_frags

    def handle_loose_operators(self, mod_contents, base_path):
        loc_op_triple       =  [(base_path, x, y) for x,y in mod_contents if applicable(y, (ProductionOperator, ActionOperator))]
        instanced_operators =  [(mod, name, y() if needs_init(y) else y) for mod, name, y in loc_op_triple]
        words_op            =  [(prep_op_path(mod, name), y) for mod, name, y in instanced_operators]
        op_sens             =  [VF.sen(xs).attach_statement(y) for xs, y in words_op]
        return op_sens
