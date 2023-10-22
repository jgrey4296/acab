##-- imports
from __future__ import annotations
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.printing.printers as Printers
from acab.interfaces.printing import PrintSemantics_i, PrintSystem_i
from acab.modules.printing.basic_printer  import BasicPrinter
import acab
from acab.core.defaults import print_signals as DSig

##-- end imports

config = acab.config

atom_pr               = BasicPrinter.Spec(DSig.ATOM).spec_from(PrintSemantics_i)
annotations_pr        = BasicPrinter.Spec(DSig.ANNOTATIONS).spec_from(PrintSemantics_i)
annotations_final_pr  = BasicPrinter.Spec(DSig.ANNOTATIONS_FINAL).spec_from(PrintSemantics_i)
sentence_pr           = BasicPrinter.Spec(DSig.SENTENCE).spec_from(PrintSemantics_i)
symbol_pr             = BasicPrinter.Spec(DSig.SYMBOL).spec_from(PrintSemantics_i)
constraint_pr         = BasicPrinter.Spec(DSig.CONSTRAINT).spec_from(PrintSemantics_i)
container_pr          = BasicPrinter.Spec(DSig.CONTAINER).spec_from(PrintSemantics_i)
implicit_container_pr = BasicPrinter.Spec(DSig.IMPLICIT_CONTAINER).spec_from(PrintSemantics_i)
modal_pr              = BasicPrinter.Spec(DSig.MODAL).spec_from(PrintSemantics_i)
component_pr          = BasicPrinter.Spec(DSig.COMPONENT).spec_from(PrintSemantics_i)
type_instance_pr      = BasicPrinter.Spec(DSig.TYPE_INSTANCE).spec_from(PrintSemantics_i)
structure_pr          = BasicPrinter.Spec(DSig.STRUCTURE).spec_from(PrintSemantics_i)
tags_pr               = BasicPrinter.Spec(DSig.TAGS).spec_from(PrintSemantics_i)

def DEFAULT_PRINTER_SPEC():
    return [atom_pr,
            annotations_pr,
            annotations_final_pr,
            sentence_pr,
            symbol_pr,
            constraint_pr,
            container_pr,
            implicit_container_pr,
            modal_pr,
            component_pr,
            type_instance_pr,
            structure_pr,
            tags_pr]

def DEFAULT_PRINT_HANDLERS():
    return [
        # Printers.NoOpPrinter().as_handler(signal="TYPE_INSTANCE"),
        Printers.AnnotationAwareValuePrinter().as_handler(signal=DSig.ATOM),
        Printers.AnnotationPrinter().as_handler(signal=DSig.ANNOTATIONS),
        Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
        Printers.BasicSentenceAwarePrinter().as_handler(signal=DSig.SENTENCE),
        Printers.ConfigBackedSymbolPrinter().as_handler(signal=DSig.SYMBOL),
        Printers.ConstraintPrinter().as_handler(signal=DSig.CONSTRAINT),
        Printers.ExplicitContainerPrinter().as_handler(signal=DSig.CONTAINER),
        Printers.ImplicitContainerPrinter().as_handler(signal=DSig.IMPLICIT_CONTAINER),
        Printers.ModalPrinter().as_handler(signal=DSig.MODAL),
        Printers.ProductionComponentPrinter().as_handler(signal=DSig.COMPONENT),
        Printers.SimpleTypePrinter().as_handler(signal=DSig.TYPE_INSTANCE),
        Printers.StructurePrinter().as_handler(signal=DSig.STRUCTURE),
        Printers.TagPrinter().as_handler(signal=DSig.TAGS),
    ]

def DEFAULT_PRINTER():
    return BasicPrinter(init_specs=DEFAULT_PRINTER_SPEC(),
                        init_handlers=DEFAULT_PRINT_HANDLERS(),
                        settings={"MODAL": "exop"})
