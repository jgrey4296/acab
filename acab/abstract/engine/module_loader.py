"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from types import ModuleType
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import GET
from acab.abstract.core.production_abstractions import ProductionOperator
from acab.abstract.engine.util import applicable, needs_init, usable
from acab.abstract.interfaces.dsl import DSL_Fragment_i
from acab.abstract.interfaces.module_loader import (ModuleComponents,
                                                    ModuleLoader_i)
from acab.abstract.interfaces.printing import PrintSemantics_i
from acab.abstract.interfaces.semantic import (AbstractionSemantics_i,
                                               DependentSemantics_i,
                                               IndependentSemantics_i)
from acab.abstract.core.values import Sentence
import re

config = GET()

MODULE_SPLIT_REG = re.compile(config.prepare("Parse.Patterns", "MODULE_SPLIT_REG")())

#--------------------
class ModuleLoader(ModuleLoader_i):
    """ Describes how an engine loads ACAB/py modules """

    def extract_from_module(self, module: ModuleType) -> ModuleComponents:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        base_path      = module.__package__
        reference_path = MODULE_SPLIT_REG.split(module.__name__)
        queue          = [(base_path, module)]
        dsl_fragments  = []
        semantics      = []
        operators      = []
        printers       = []

        while bool(queue):
            curr_path, curr_mod = queue.pop(0)

            # Ignore dunders
            mod_contents        =  [(x, getattr(curr_mod, x)) for x in dir(curr_mod) if "__" not in x]

            # queue submodules
            sub_modules         =  [(y.__package__, y) for x,y in mod_contents if isinstance(y, ModuleType)]
            queue               += [(x,y) for x,y in sub_modules if base_path in y.__package__ and "__init__" in y.__file__]

            # Get module dsl_fragments
            available_dsls      =  [y for x,y in mod_contents if applicable(y, DSL_Fragment_i)]
            dsl_fragments       += [y() if needs_init(y) else y for y in available_dsls]

            # Get Semantics
            # TODO shift to a semantic fragment system like dsls
            # TODO only let abstraction and independent semantics be built
            available_semantics =  [y for x,y in mod_contents if applicable(y, (DependentSemantics_i,
                                                                                IndependentSemantics_i,
                                                                                AbstractionSemantics_i))]
            semantics           += [y() if needs_init(y) else y for y in available_semantics]

            # Get Ops
            loc_op_pairs        =  [(reference_path + MODULE_SPLIT_REG.split(x), y) for x,y in mod_contents if applicable(y, ProductionOperator)]
            instanced_operators =  [(xs, y() if needs_init(y, ProductionOperator) else y) for xs, y in loc_op_pairs]
            sentences           =  [Sentence.build(xs).attach_statement(y) for xs, y in instanced_operators]
            operators           += sentences

            # Get printers
            available_printers  =  [y for x,y in mod_contents if usable(y, PrintSemantics_i)]
            printers            += [y() if needs_init(y) else y for y in mod_contents]



        # TODO: load any values needed for the operators?

        return ModuleComponents(dsl_fragments,
                                semantics,
                                operators,
                                printers)


