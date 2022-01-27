from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.interfaces.printing import PrintSemantics_i
from acab.modules.printing.basic_printer import BasicPrinter
import acab.modules.printing.printers as Printers

atom_pr               = BasicPrinter.Spec("ATOM").spec_from(PrintSemantics_i)
annotations_pr        = BasicPrinter.Spec("ANNOTATIONS").spec_from(PrintSemantics_i)
sentence_pr           = BasicPrinter.Spec("SENTENCE").spec_from(PrintSemantics_i)
symbol_pr             = BasicPrinter.Spec("SYMBOL").spec_from(PrintSemantics_i)
constraint_pr         = BasicPrinter.Spec("CONSTRAINT").spec_from(PrintSemantics_i)
container_pr          = BasicPrinter.Spec("CONTAINER").spec_from(PrintSemantics_i)
implicit_container_pr = BasicPrinter.Spec("IMPLICIT_CONTAINER").spec_from(PrintSemantics_i)
modal_pr              = BasicPrinter.Spec("MODAL").spec_from(PrintSemantics_i)
component_pr          = BasicPrinter.Spec("COMPONENT").spec_from(PrintSemantics_i)
type_instance_pr      = BasicPrinter.Spec("TYPE_INSTANCE").spec_from(PrintSemantics_i)
structure_pr          = BasicPrinter.Spec("STRUCTURE").spec_from(PrintSemantics_i)
tags_pr               = BasicPrinter.Spec("TAGS").spec_from(PrintSemantics_i)

def DEFAULT_PRINTER_SPEC():
    return [atom_pr,
            annotations_pr,
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
        Printers.AnnotationAwareValuePrinter().as_handler(signal="ATOM"),
        Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
        Printers.BasicSentenceAwarePrinter().as_handler(signal="SENTENCE"),
        Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
        Printers.ConstraintPrinter().as_handler(signal="CONSTRAINT"),
        Printers.ExplicitContainerPrinter().as_handler(signal="CONTAINER"),
        Printers.ImplicitContainerPrinter().as_handler(signal="IMPLICIT_CONTAINER"),
        Printers.ModalPrinter().as_handler(signal="MODAL"),
        Printers.ProductionComponentPrinter().as_handler(signal="COMPONENT"),
        Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
        Printers.StructurePrinter().as_handler(signal="STRUCTURE"),
        Printers.TagPrinter().as_handler(signal="TAGS"),
    ]

def DEFAULT_PRINTER():
    return BasicPrinter(init_specs=DEFAULT_PRINTER_SPEC(),
                        init_handlers=DEFAULT_PRINT_HANDLERS(),
                        settings={"MODAL": "exop"})
