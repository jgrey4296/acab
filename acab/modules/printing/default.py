from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.modules.printing.basic_printer import BasicPrinter
import acab.modules.printing.printers as Printers

def DEFAULT_PRINTER():
    return BasicPrinter(init_handlers=[
        # Printers.NoOpPrinter().as_handler("_:TYPE_INSTANCE"),
        Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
        Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
        Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
        Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
        Printers.ConstraintPrinter().as_handler("_:CONSTRAINT"),
        Printers.ExplicitContainerPrinter().as_handler("_:CONTAINER"),
        Printers.ImplicitContainerPrinter().as_handler("_:IMPLICIT_CONTAINER"),
        Printers.ModalPrinter().as_handler("_:MODAL")
        Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
        Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
        Printers.StructurePrinter().as_handler("_:STRUCTURE"),
        Printers.TagPrinter().as_handler("_:TAGS"),

    ],
                        settings={"MODAL": "exop"})
