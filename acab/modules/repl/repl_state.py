#!/usr/bin/env python3
from dataclasses import dataclass, field, InitVar
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import acab
config = acab.GET()

from acab.modules.repl import repl_commands as ReC

@dataclass
class ReplState:
    """ Data used for control of the repl """
    prompt        : str       =  field(default=config("REPL", "PROMPT"))
    prompt_ml     : str       =  field(default=config("REPL", "PROMPT_ML"))
    command       : Callable  =  field(default=ReC.ReplE.NOP)
    params        : List[str] =  field(default_factory=list)
    result        : Any       =  field(default=None)
    current_str   : str       =  field(default=None)
    collect_str   : List[str] =  field(default_factory=list)
    echo          : bool      =  field(default=False)
    stack         : bool      =  field(default=False)
    in_multi_line : bool      =  field(default=False)
