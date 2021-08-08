#!/opts/anaconda3/envs/ENV/python
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir

from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.parsing.exlo.el_dsl import EL_Parser
from acab.modules.printing.basic_printer import BasicPrinter
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.default import DEFAULT_SEMANTICS


basic = AcabBasicEngine(parser=EL_Parser(),
                        semantics=DEFAULT_SEMANTICS(),
                        printer=DEFAULT_PRINTER)
