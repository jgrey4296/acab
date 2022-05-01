#!/usr/bin/env python3
import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Type, Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from collections import defaultdict, deque
from unittest.mock import DEFAULT
from uuid import UUID, uuid1

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

REGISTER = "REGISTER"

@dataclass
class PrintSystemImpl(HS.HandlerSystem, PI.PrintSystem_i):

    print_registers : dict[UUID, list[str]] = field(init=False, default_factory=lambda: defaultdict(lambda: list()))
    total : ClassVar[int]  = 0

    def __post_init__(self, specs, handlers, sieve_fns) -> None: #type:ignore[no-untyped-def]
        # TODO abstract this into a method?
        super().__post_init__(specs, handlers, sieve_fns) #type:ignore[no-untyped-call]
        if not bool(self.handler_specs[DEFAULT_HANDLER_SIGNAL]):
            default = HS.Handler(DEFAULT_HANDLER_SIGNAL, func=lambda x, data=None: str(x))
            self.register(default) #type:ignore[no-untyped-call]

    def __call__(self, *args:Sentence) -> str:
        return self.pprint(*args)

    def __repr__(self):
        return f"<{self.__class__.__name__}: {len(self.handler_specs)} handlers, {len(self.sieve)} sieves>"

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
        logging.info("Initiating pprint on {} sentences", len(args))
        self.print_registers.clear()
        remaining = deque([[x, self.separator] for x in args[:-1]] + [args[-1]]) #type:ignore
        result    = []
        while bool(remaining):
            PrintSystemImpl.total += 1
            current = remaining.popleft()
            target  = current
            spec    = None
            data    : dict[str, Any] = {}
            # flatten and get spec
            match current:
                case str():
                    result.append(current)
                    continue
                case list():
                    remaining.extendleft(reversed(current))
                    continue
                case HSi.HandlerOverride(signal="_") if (isinstance(current.value, str) and
                                                         REGISTER in current.data):
                    self.print_registers[current.data[REGISTER]].append(current.value)
                    continue
                case HSi.HandlerOverride():
                    spec = self.lookup(current)
                    data.update(current.data)
                    target = current.value
                case _:
                    spec = self.lookup(current)

            logging.debug("(Remain/Total:{:3}/{:4}) Calling: {:>15} : {}", len(remaining), PrintSystemImpl.total, str(spec), target)
            # Run spec
            is_default = spec.signal == DEFAULT_HANDLER_SIGNAL
            if  not is_default and spec.check_api(func=PI.PrintSemantics_i):
                handled = spec[0](target, top=self, data=data)
            else:
                handled = spec[0](target, data=data)

            # Add the results of a handler to the head,
            # and propagate registers
            match (REGISTER in data, handled):
                case (False, list()):
                    remaining.extendleft(reversed(handled))
                case (True, list()):
                    wrapped = [self.assign_to_register(False, x, data)[1] for x in handled]
                    remaining.extendleft(reversed(wrapped))
                case (True, x):
                    remaining.appendleft(self.assign_to_register(False, x, data)[1])
                case (False, x):
                    remaining.appendleft(x)

        return "".join(result)


    def extend(self, mods:list[ModuleComponents]) -> None:
        logging.info("Extending Printer")
        printers = [y for x in mods for y in x.printers]
        assert(all([isinstance(x, PI.Printer_Fragment_i) for x in printers]))
        for print_fragment in printers:
            assert(print_fragment.target_i is None or issubclass(print_fragment.target_i, PI.PrintSystem_i)) #type:ignore
            for val in print_fragment:
                self.register(val) #type:ignore[no-untyped-call]



    def assign_to_register(self, signal, value, existing_data=None):
        if existing_data and REGISTER in existing_data:
            reg = existing_data[REGISTER]
        else:
            reg    = uuid1()
        data = {REGISTER: reg}
        overridden = self.override(signal, value, data=data)
        return reg, overridden

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


class PrinterFragment(HS.HandlerFragment, PI.Printer_Fragment_i):
    pass
