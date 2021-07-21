from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.modules.printing.basic_printer import BasicPrinter
import acab.modules.printing.printers as Printers

DEFAULT_PRINTER = BasicPrinter(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                         Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                         Printers.ProductionComponentPrinter("_:COMPONENT"),
                                         Printers.ExplicitContainerPrinter("_:CONTAINER"),
                                         Printers.ImplicitContainerPrinter("_:IMPLICIT_CONTAINER"),
                                         Printers.StructurePrinter("_:STRUCTURE"),
                                         Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                         Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL")],
                               structs=[],
                               settings={"MODAL": "exop"})
