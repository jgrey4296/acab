#!/opts/anaconda3/envs/ENV/python
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, cast)

from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.printing.basic_printer import BasicPrinter
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.default import (DEFAULT_SEMANTICS,
                                            EXLO_PROXY_SEMANTICS,
                                            EXLO_SEMANTICS)



def basic():
    return AcabBasicEngine(parser=EXLO_Parser,
                           semantics=DEFAULT_SEMANTICS(),
                           printer=DEFAULT_PRINTER())

def exlo():
    return AcabBasicEngine(parser=EXLO_Parser,
                           semantics=EXLO_SEMANTICS(),
                           printer=DEFAULT_PRINTER())

def exlo_proxy():
    return AcabBasicEngine(parser=EXLO_Parser,
                           semantics=EXLO_PROXY_SEMANTICS(),
                           printer=DEFAULT_PRINTER())
