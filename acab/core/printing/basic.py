#!/usr/bin/env python3
import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Type, Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import GET
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.printing.default_symbols import PRINT_SEPARATOR_P
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.value import Sentence_i, Value_i
from acab.interfaces import handler_system as HSi
from acab.interfaces import printing as PI
import acab.core.util.part_implementations.handler_system as HS

logging                      = logmod.getLogger(__name__)
config                       = GET()
DEFAULT_HANDLER_SIGNAL       = config.prepare("Handler.System", "DEFAULT_SIGNAL")()
Sentence         : TypeAlias = AT.Sentence
ModuleComponents : TypeAlias = AT.ModuleComponents
GenFunc          : TypeAlias = AT.fns.GenFunc

class PrintSystemImpl(HS.HandlerSystem, PI.PrintSystem_i):

    total : ClassVar[int]  = 0

    def __post_init__(self, specs, handlers, sieve_fns) -> None: #type:ignore[no-untyped-def]
        # TODO abstract this into a method?
        super().__post_init__(specs, handlers, sieve_fns) #type:ignore[no-untyped-call]
        if not bool(self.handler_specs[DEFAULT_HANDLER_SIGNAL]):
            default = HS.Handler(DEFAULT_HANDLER_SIGNAL, lambda x, data=None: str(x))
            self.register(default) #type:ignore[no-untyped-call]

    def __call__(self, *args:Sentence) -> str:
        return self.pprint(*args)

    def check(self, val:str) -> None | str:
        """ Check a value to toggle variations/get defaults"""
        if val in self.settings:
            return self.settings[val]

        return None

    def pprint(self, *args:Sentence) -> str:
        """
        The Core Pretty Printer.
        Process a Stack, looking up specific handlers for the top,
        building up a final string

        Handler's return a list of values to go onto stack.
        """
        remaining = [[x, self.separator] for x in args[:-1]] + [args[-1]] #type:ignore
        result = []
        while bool(remaining):
            PrintSystemImpl.total += 1
            current = remaining.pop(0)
            spec    = None
            data    : dict[str, Any] = {}
            if isinstance(current, str):
                result.append(current)
                continue
            elif isinstance(current, list):
                remaining = current + remaining
                continue
            else:
                spec = self.lookup(current)

            if isinstance(current, HSi.HandlerOverride):
                data.update(current.data)
                current = current.value

            if spec.check_api(func=PI.PrintSemantics_i):
                logging.debug("(Remain:{:3}/{:4}) Calling: {:>15} : {}", len(remaining), PrintSystemImpl.total, str(spec), current)
                handled = spec[0](current, top=self, data=data)
            else:
                handled = spec[0](current, data=data)

            if not isinstance(handled, list):
                handled = [handled]

            # Add the results of a handler to the head
            remaining = handled + remaining


        return "".join(result)


    def extend(self, mods:list[ModuleComponents]) -> None:
        logging.info("Extending Printer")
        printers = [y for x in mods for y in x.printers]
        assert(all([isinstance(x, PI.Printer_Fragment) for x in printers]))
        for print_fragment in printers:
            assert(print_fragment.target_i is None or issubclass(print_fragment.target_i, PrintSystem_i)) #type:ignore
            for val in print_fragment:
                self.register(val) #type:ignore[no-untyped-call]


class PrintSemanticsImpl(HS.HandlerComponent, PI.PrintSemantics_i):

    transforms : list[GenFunc]

    def __post_init__(self) -> None:
        self.transforms += self.add_transforms()

    def add_transforms(self) -> list[GenFunc]:
        """ Override to add custom transforms in a class """
        return []

    def run_transforms(self, value: Value_i, curr_str: list[Any]) -> list[Any]:
        curr = curr_str
        for trans in self.transforms:
            curr = trans(self, value, curr)

        return curr

    def verify(self, instruction:AT.Value) -> bool:
        return True

