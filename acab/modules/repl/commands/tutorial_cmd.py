#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass


from acab.modules.repl.repl_commander import register_class


@register_class("tutorial")
class TutorialCmd:
    """
    Print out a basic tutorial of Acab and this Repl
    """

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        pass


    def __call__(self, line):
        # Print a section, return to main loop,
        # if tutorial is called again, continue
        # if restart is passed in, restart the tutorial
        # also include option to print tutorial for a module
        return
