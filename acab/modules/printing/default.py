from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.modules.printing.basic_printer import BasicPrinter
import acab.modules.printing.printers as Printers

def DEFAULT_PRINTER():
    return BasicPrinter(init_handlers=[
        Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
        Printers.ConstraintAwareValuePrinter().as_handler("_:ATOM"),
        Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
        Printers.ExplicitContainerPrinter().as_handler("_:CONTAINER"),
        Printers.ImplicitContainerPrinter().as_handler("_:IMPLICIT_CONTAINER"),
        Printers.StructurePrinter().as_handler("_:STRUCTURE"),
        Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
        Printers.PrimitiveTypeAwarePrinter().as_handler("_:NO_MODAL")
    ],
                        settings={"MODAL": "exop"})
