from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.printing.printers as Printers
from acab.interfaces.printing import PrintSemantics_i, PrintSystem_i
from acab.modules.printing.basic_printer  import BasicPrinter
from acab.core.config.config import AcabConfig
from acab.core.printing import default_signals as DS

config = AcabConfig()

atom_pr               = BasicPrinter.Spec(DS.ATOM).spec_from(PrintSemantics_i)
annotations_pr        = BasicPrinter.Spec(DS.ANNOTATIONS).spec_from(PrintSemantics_i)
annotations_final_pr  = BasicPrinter.Spec(DS.ANNOTATIONS_FINAL).spec_from(PrintSemantics_i)
sentence_pr           = BasicPrinter.Spec(DS.SENTENCE).spec_from(PrintSemantics_i)
symbol_pr             = BasicPrinter.Spec(DS.SYMBOL).spec_from(PrintSemantics_i)
constraint_pr         = BasicPrinter.Spec(DS.CONSTRAINT).spec_from(PrintSemantics_i)
container_pr          = BasicPrinter.Spec(DS.CONTAINER).spec_from(PrintSemantics_i)
implicit_container_pr = BasicPrinter.Spec(DS.IMPLICIT_CONTAINER).spec_from(PrintSemantics_i)
modal_pr              = BasicPrinter.Spec(DS.MODAL).spec_from(PrintSemantics_i)
component_pr          = BasicPrinter.Spec(DS.COMPONENT).spec_from(PrintSemantics_i)
type_instance_pr      = BasicPrinter.Spec(DS.TYPE_INSTANCE).spec_from(PrintSemantics_i)
structure_pr          = BasicPrinter.Spec(DS.STRUCTURE).spec_from(PrintSemantics_i)
tags_pr               = BasicPrinter.Spec(DS.TAGS).spec_from(PrintSemantics_i)

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
        Printers.AnnotationAwareValuePrinter().as_handler(signal=DS.ATOM),
        Printers.AnnotationPrinter().as_handler(signal=DS.ANNOTATIONS),
        Printers.AnnotationFinaliser().as_handler(signal=DS.ANNOTATIONS_FINAL),
        Printers.BasicSentenceAwarePrinter().as_handler(signal=DS.SENTENCE),
        Printers.ConfigBackedSymbolPrinter().as_handler(signal=DS.SYMBOL),
        Printers.ConstraintPrinter().as_handler(signal=DS.CONSTRAINT),
        Printers.ExplicitContainerPrinter().as_handler(signal=DS.CONTAINER),
        Printers.ImplicitContainerPrinter().as_handler(signal=DS.IMPLICIT_CONTAINER),
        Printers.ModalPrinter().as_handler(signal=DS.MODAL),
        Printers.ProductionComponentPrinter().as_handler(signal=DS.COMPONENT),
        Printers.SimpleTypePrinter().as_handler(signal=DS.TYPE_INSTANCE),
        Printers.StructurePrinter().as_handler(signal=DS.STRUCTURE),
        Printers.TagPrinter().as_handler(signal=DS.TAGS),
    ]

def DEFAULT_PRINTER():
    return BasicPrinter(init_specs=DEFAULT_PRINTER_SPEC(),
                        init_handlers=DEFAULT_PRINT_HANDLERS(),
                        sieve_fns=[],
                        settings={"MODAL": "exop"})
