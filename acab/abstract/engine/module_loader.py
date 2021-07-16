"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.engine.util import (ModuleComponents, applicable_comp,
                                       comp_needs_instantiation, usable_comp)
from acab.abstract.interfaces.dsl_interface import DSL_Interface
from acab.abstract.interfaces.module_loader_interface import ModuleLoader_Interface
from acab.abstract.interfaces.printing_interfaces import PrintSemantics
from acab.abstract.interfaces.semantic_interfaces import (AbstractionSemantics,
                                                          DependentSemantics,
                                                          IndependentSemantics)

ModuleType   = 'Module'
Sentence     = 'Sentence'
DSL_Fragment = DSL_Interface


#--------------------
class ModuleLoader(ModuleLoader_Interface):
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
            mod_contents = [(x, getattr(curr_mod, x)) for x in dir(curr_mod) if "__" not in x]

            # queue submodules
            sub_modules = [(y.__package__, y) for x,y in mod_contents if isinstance(y, ModuleType)]
            queue += [(x,y) for x,y in sub_modules if base_path in y.__package__ and "__init__" in y.__file__]

            # Get module dsl_fragments
            dsl_fragments += [y for x,y in mod_contents if usable_comp(y, DSL_Interface)]
            dsl_fragments += [y() for x,y in mod_contents if comp_needs_instantiation(y, DSL_Interface)]

            # Get Semantics
            semantics     += [y for x,y in mod_contents if usable_comp(y, (DependentSemantics, IndependentSemantics, AbstractionSemantics))]
            semantics     += [y() for x,y in mod_contents if comp_needs_instantiation(y, (DependentSemantics, IndependentSemantics, AbstractionSemantics))]

            # Get Ops
            loc_op_pairs = [(reference_path + MODULE_SPLIT_REG.split(x), y) for x,y in mod_contents if applicable_comp(y, ProductionOperator)]
            instanced_operators = [(xs, y() if comp_needs_instantiation(y, ProductionOperator) else y) for xs, y in loc_op_pairs]
            sentences = [Sentence.build(xs).attach_statement(y) for xs, y in instanced_operators]
            operators += sentences

            # Get printers
            printers     += [y for x,y in mod_contents if usable_comp(y, PrintSemantics)]
            printers     += [y() for x,y in mod_contents if comp_needs_instantiation(y, PrintSemantics)]



        # TODO: load any values needed for the operators?

        return ModuleComponents(dsl_fragments,
                                semantics,
                                operators,
                                printers)


