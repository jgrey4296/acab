from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.modules.printing.basic_printer import BasicPrinter
import acab.modules.printing.printers as Printers
from acab.abstract.interfaces.handler_system import Handler

def DEFAULT_PRINTER():
    return BasicPrinter(in_handlers=[
        Handler("_:SENTENCE",           Printers.BasicSentenceAwarePrinter),
        Handler("_:ATOM",               Printers.ConstraintAwareValuePrinter),
        Handler("_:COMPONENT",          Printers.ProductionComponentPrinter),
        Handler("_:CONTAINER",          Printers.ExplicitContainerPrinter),
        Handler("_:IMPLICIT_CONTAINER", Printers.ImplicitContainerPrinter),
        Handler("_:STRUCTURE",          Printers.StructurePrinter),
        Handler("_:SYMBOL",             Printers.ConfigBackedSymbolPrinter),
        Handler("_:NO_MODAL",           Printers.PrimitiveTypeAwarePrinter),
    ],
                        settings={"MODAL": "exop"})
