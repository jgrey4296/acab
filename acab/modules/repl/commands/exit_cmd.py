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


@register_class("exit")
class ExitCmd:
    """
    Exit the repl, automatically saving the self state
    """

    def __call__(self, line):
        # TODO add option for saving location
        logging.info("Quitting")
        filename = "repl.auto"
        self._repl.state.engine.save_file(filename)
        return True
