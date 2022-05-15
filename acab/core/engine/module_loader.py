"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
import logging as logmod
import re
from dataclasses import dataclass, field
from types import ModuleType
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab import AcabConfig
from acab.core.value.instruction import ProductionOperator, ActionOperator
from acab.core.value.sentence import Sentence
from acab.core.engine.util import applicable, needs_init, prep_op_path, ensure_handler

from acab.interfaces.module_loader import (ModuleComponents,
                                           ModuleLoader_i)
from acab.interfaces import fragments as FI
from acab.core.engine.module_loader_base import ModuleLoaderBase

config           = AcabConfig()
MODULE_SPLIT_REG = re.compile(config.prepare("Parse.Patterns", "MODULE_SPLIT_REG")())

#--------------------
class ModuleLoader(ModuleLoaderBase, ModuleLoader_i):
    """ Describes how an engine loads ACAB/py modules """

    def extract_from_module(self, module: ModuleType) -> ModuleComponents:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        logging.info("Extracting module: {}", module)
        base_path      = module.__package__
        # reference_path = MODULE_SPLIT_REG.split(module.__name__)
        queue          = [(base_path, module)]
        dsl_fragments  : list[FI.DSL_Fragment_i]     = []
        semantic_frags : list[FI.Semantic_Fragment_i]  = []
        printers       : list[FI.Printer_Fragment_i]   = []
        operators      : list[ProductionOperator] = []

        # TODO extract *handlers* not semantics
        while bool(queue):
            curr_path, curr_mod = queue.pop(0)

            # Ignore dunders
            mod_contents        =  [(x, getattr(curr_mod, x)) for x in dir(curr_mod) if "__" not in x]

            # queue submodules
            sub_modules         =  [(y.__package__, y) for x,y in mod_contents if isinstance(y, ModuleType)]
            queue               += [(x,y) for x,y in sub_modules if base_path in y.__package__ and "__init__" in y.__file__]

            # Get module dsl_fragments
            available_dsls      =  [y for x,y in mod_contents if applicable(y, FI.DSL_Fragment_i)]
            dsl_fragments       += [y() if needs_init(y) else y for y in available_dsls]

            # Get Semantics
            available_sem_frags =  [y for x,y in mod_contents if applicable(y, FI.Semantic_Fragment_i)]
            semantic_frags      =  [y() if needs_init(y) else y for y in available_sem_frags]

            # Get Ops
            loc_op_triple       =  [(base_path, x, y) for x,y in mod_contents if applicable(y, (ProductionOperator, ActionOperator))]
            instanced_operators =  [(mod, name, y() if needs_init(y) else y) for mod, name, y in loc_op_triple]
            words_op            =  [(prep_op_path(mod, name), y) for mod, name, y in instanced_operators]
            sentences           =  [Sentence(xs).attach_statement(y) for xs, y in words_op]
            operators           += sentences

            # Get printers
            available_printers  =  [y for x,y in mod_contents if applicable(y, FI.Printer_Fragment_i) and not isinstance(y, type(object))]
            printers            +=  available_printers



        # End of while
        result = ModuleComponents(str(module.__package__),
                                  dsl_fragments,
                                  semantic_frags,
                                  printers,
                                  operators)

        logging.debug(f"Extracted: {result}")

        return result
