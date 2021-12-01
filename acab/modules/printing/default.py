from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.interfaces.printing import PrintSemantics_i
from acab.modules.printing.basic_printer import BasicPrinter
import acab.modules.printing.printers as Printers

atom_pr               = BasicPrinter.Spec("_:ATOM").spec_from(PrintSemantics_i)
annotations_pr        = BasicPrinter.Spec("_:ANNOTATIONS").spec_from(PrintSemantics_i)
sentence_pr           = BasicPrinter.Spec("_:SENTENCE").spec_from(PrintSemantics_i)
symbol_pr             = BasicPrinter.Spec("_:SYMBOL").spec_from(PrintSemantics_i)
constraint_pr         = BasicPrinter.Spec("_:CONSTRAINT").spec_from(PrintSemantics_i)
container_pr          = BasicPrinter.Spec("_:CONTAINER").spec_from(PrintSemantics_i)
implicit_container_pr = BasicPrinter.Spec("_:IMPLICIT_CONTAINER").spec_from(PrintSemantics_i)
modal_pr              = BasicPrinter.Spec("_:MODAL").spec_from(PrintSemantics_i)
component_pr          = BasicPrinter.Spec("_:COMPONENT").spec_from(PrintSemantics_i)
type_instance_pr      = BasicPrinter.Spec("_:TYPE_INSTANCE").spec_from(PrintSemantics_i)
structure_pr          = BasicPrinter.Spec("_:STRUCTURE").spec_from(PrintSemantics_i)
tags_pr               = BasicPrinter.Spec("_:TAGS").spec_from(PrintSemantics_i)

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
        # Printers.NoOpPrinter().as_handler("_:TYPE_INSTANCE"),
        Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
        Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
        Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
        Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
        Printers.ConstraintPrinter().as_handler("_:CONSTRAINT"),
        Printers.ExplicitContainerPrinter().as_handler("_:CONTAINER"),
        Printers.ImplicitContainerPrinter().as_handler("_:IMPLICIT_CONTAINER"),
        Printers.ModalPrinter().as_handler("_:MODAL"),
        Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
        Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
        Printers.StructurePrinter().as_handler("_:STRUCTURE"),
        Printers.TagPrinter().as_handler("_:TAGS"),
    ]

def DEFAULT_PRINTER():
    return BasicPrinter(init_specs=DEFAULT_PRINTER_SPEC(),
                        init_handlers=DEFAULT_PRINT_HANDLERS(),
                        settings={"MODAL": "exop"})
